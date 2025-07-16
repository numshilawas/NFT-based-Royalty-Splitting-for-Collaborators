  (define-constant contract-owner tx-sender)
  (define-constant err-owner-only (err u100))
  (define-constant err-not-found (err u101))
  (define-constant err-invalid-percentage (err u102))
  (define-constant err-already-exists (err u103))

  (define-map collaborators-map 
    { nft-id: uint, collaborator: principal }
    { share-percentage: uint }
  )

  (define-map nft-metadata
    { nft-id: uint }
    { 
      title: (string-ascii 50),
      description: (string-ascii 200),
      total-shares: uint,
      creator: principal
    }
  )

  (define-data-var nft-counter uint u0)

  (define-read-only (get-nft-metadata (nft-id uint))
    (map-get? nft-metadata { nft-id: nft-id })
  )

  (define-non-fungible-token collaborative-nft uint)

  (define-public (mint-collaborative-nft (title (string-ascii 50)) (description (string-ascii 200)))
    (let 
      (
        (new-id (+ (var-get nft-counter) u1))
      )
      (try! (nft-mint? collaborative-nft new-id tx-sender))
      (map-set nft-metadata
        { nft-id: new-id }
        {
          title: title,
          description: description,
          total-shares: u0,
          creator: tx-sender
        }
      )
      (var-set nft-counter new-id)
      (ok new-id)
    )
  )

  (define-public (add-collaborator (nft-id uint) (collaborator principal) (share-percentage uint))
    (let
      (
        (current-metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
        (new-total-shares (+ (get total-shares current-metadata) share-percentage))
      )
      (asserts! (is-eq tx-sender (get creator current-metadata)) err-owner-only)
      (asserts! (<= new-total-shares u100) err-invalid-percentage)
      (asserts! (is-none (map-get? collaborators-map { nft-id: nft-id, collaborator: collaborator })) err-already-exists)
      (map-set collaborators-map
        { nft-id: nft-id, collaborator: collaborator }
        { share-percentage: share-percentage }
      )
      (map-set nft-metadata
        { nft-id: nft-id }
        (merge current-metadata { total-shares: new-total-shares })
      )
      (ok true)
    )
  )

  ;; Remove duplicate - kept the complete implementation below

  (define-public (update-share (nft-id uint) (collaborator principal) (new-share uint))
    (let
      (
        (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
        (current-share (unwrap! (map-get? collaborators-map { nft-id: nft-id, collaborator: collaborator }) err-not-found))
        (old-total (get total-shares metadata))
        (new-total (+ (- old-total (get share-percentage current-share)) new-share))
      )
      (asserts! (is-eq tx-sender (get creator metadata)) err-owner-only)
      (asserts! (<= new-total u100) err-invalid-percentage)
      (map-set collaborators-map
        { nft-id: nft-id, collaborator: collaborator }
        { share-percentage: new-share }
      )
      (map-set nft-metadata
        { nft-id: nft-id }
        (merge metadata { total-shares: new-total })
      )
      (ok true)
    )
  )

  (define-public (transfer-nft (nft-id uint) (sender principal) (recipient principal))
    (begin
      (asserts! (is-eq tx-sender sender) err-owner-only)
      (try! (nft-transfer? collaborative-nft nft-id sender recipient))
      (ok true)
    )
  )

(define-public (distribute-royalty (nft-id uint) (amount uint))
  (let
    (
      (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
    )
    (asserts! (is-eq (get total-shares metadata) u100) err-invalid-percentage)
    (unwrap! (distribute-to-collaborators nft-id amount) err-invalid-percentage)
    (ok true)
  )
)

(define-private (distribute-to-collaborators (nft-id uint) (total-amount uint))
  (let
    (
      (collaborator-list (get-all-collaborators nft-id))
    )
    (ok (fold distribute-single-payment collaborator-list total-amount))
  )
)

(define-private (distribute-single-payment 
  (collaborator-data { nft-id: uint, collaborator: principal, share: uint })
  (prev-result uint)
)
  (let
    (
      (total-amount prev-result)
      (payment-amount (/ (* total-amount (get share collaborator-data)) u100))
    )
    (if (> payment-amount u0)
      (begin
        ;; In a real implementation, handle the stx-transfer properly
        total-amount
      )
      total-amount
    )
  )
)

(define-private (get-all-collaborators (nft-id uint))
  (list { nft-id: nft-id, collaborator: tx-sender, share: u0 })
)

(define-map collaborator-payments
  { nft-id: uint, collaborator: principal }
  { total-received: uint, last-payment: uint }
)

(define-read-only (get-collaborator-earnings (nft-id uint) (collaborator principal))
  (map-get? collaborator-payments { nft-id: nft-id, collaborator: collaborator })
)

(define-map marketplace-listings
  { nft-id: uint }
  { 
    seller: principal,
    price: uint,
    royalty-percentage: uint,
    active: bool
  }
)

(define-map sales-history
  { nft-id: uint, sale-id: uint }
  {
    seller: principal,
    buyer: principal,
    price: uint,
    royalty-paid: uint,
    block-height: uint
  }
)

(define-data-var sale-counter uint u0)

(define-public (list-nft (nft-id uint) (price uint) (royalty-percentage uint))
  (let
    (
      (nft-owner (unwrap! (nft-get-owner? collaborative-nft nft-id) err-not-found))
    )
    (asserts! (is-eq tx-sender nft-owner) err-owner-only)
    (asserts! (<= royalty-percentage u20) err-invalid-percentage)
    (map-set marketplace-listings
      { nft-id: nft-id }
      {
        seller: tx-sender,
        price: price,
        royalty-percentage: royalty-percentage,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (buy-nft (nft-id uint))
  (let
    (
      (listing (unwrap! (map-get? marketplace-listings { nft-id: nft-id }) err-not-found))
      (seller (get seller listing))
      (price (get price listing))
      (royalty-rate (get royalty-percentage listing))
      (royalty-amount (/ (* price royalty-rate) u100))
      (seller-amount (- price royalty-amount))
      (sale-id (+ (var-get sale-counter) u1))
    )
    (asserts! (get active listing) err-not-found)
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? seller-amount tx-sender seller)))
    (if (> royalty-amount u0)
      (try! (as-contract (distribute-royalty nft-id royalty-amount)))
      true
    )
    (try! (nft-transfer? collaborative-nft nft-id seller tx-sender))
    (map-set marketplace-listings
      { nft-id: nft-id }
      (merge listing { active: false })
    )
    (map-set sales-history
      { nft-id: nft-id, sale-id: sale-id }
      {
        seller: seller,
        buyer: tx-sender,
        price: price,
        royalty-paid: royalty-amount,
        block-height: stacks-block-height
      }
    )
    (var-set sale-counter sale-id)
    (ok true)
  )
)

(define-public (cancel-listing (nft-id uint))
  (let
    (
      (listing (unwrap! (map-get? marketplace-listings { nft-id: nft-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get seller listing)) err-owner-only)
    (asserts! (get active listing) err-not-found)
    (map-set marketplace-listings
      { nft-id: nft-id }
      (merge listing { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-listing (nft-id uint))
  (map-get? marketplace-listings { nft-id: nft-id })
)

(define-read-only (get-sale-history (nft-id uint) (sale-id uint))
  (map-get? sales-history { nft-id: nft-id, sale-id: sale-id })
)