  (define-constant contract-owner tx-sender)
  (define-constant err-owner-only (err u100))
  (define-constant err-not-found (err u101))
  (define-constant err-invalid-percentage (err u102))
  (define-constant err-already-exists (err u103))
  (define-constant err-vesting-not-started (err u104))
  (define-constant err-no-vested-amount (err u105))

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
        (distribute-to-pool (get nft-id collaborator-data) (get collaborator collaborator-data) payment-amount)
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

  (define-map vesting-schedules
    { nft-id: uint, collaborator: principal }
    {
      total-amount: uint,
      start-block: uint,
      duration-blocks: uint,
      claimed-amount: uint
    }
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
      (dynamic-royalty-rate (unwrap! (get-current-royalty-rate nft-id) err-invalid-percentage))
      (royalty-rate dynamic-royalty-rate)
      (royalty-amount (/ (* price royalty-rate) u100))
      (seller-amount (- price royalty-amount))
      (sale-id (+ (var-get sale-counter) u1))
    )
    (asserts! (get active listing) err-not-found)
    (initialize-sales-performance nft-id)
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? seller-amount tx-sender seller)))
    (if (> royalty-amount u0)
      (try! (as-contract (distribute-royalty nft-id royalty-amount)))
      true
    )
    (try! (nft-transfer? collaborative-nft nft-id seller tx-sender))
    (update-sales-performance nft-id price)
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

(define-public (create-vesting-schedule (nft-id uint) (collaborator principal) (total-amount uint) (duration-blocks uint))
  (let
    (
      (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator metadata)) err-owner-only)
    (asserts! (> duration-blocks u0) err-invalid-percentage)
    (asserts! (> total-amount u0) err-invalid-percentage)
    (map-set vesting-schedules
      { nft-id: nft-id, collaborator: collaborator }
      {
        total-amount: total-amount,
        start-block: stacks-block-height,
        duration-blocks: duration-blocks,
        claimed-amount: u0
      }
    )
    (ok true)
  )
)

(define-read-only (calculate-vested-amount (nft-id uint) (collaborator principal))
  (let
    (
      (schedule (unwrap! (map-get? vesting-schedules { nft-id: nft-id, collaborator: collaborator }) err-not-found))
      (current-block stacks-block-height)
      (start-block (get start-block schedule))
      (duration (get duration-blocks schedule))
      (total-amount (get total-amount schedule))
      (elapsed-blocks (if (> current-block start-block) (- current-block start-block) u0))
    )
    (if (>= elapsed-blocks duration)
      (ok total-amount)
      (ok (/ (* total-amount elapsed-blocks) duration))
    )
  )
)

(define-public (claim-vested-royalties (nft-id uint))
  (let
    (
      (schedule (unwrap! (map-get? vesting-schedules { nft-id: nft-id, collaborator: tx-sender }) err-not-found))
      (vested-amount (unwrap! (calculate-vested-amount nft-id tx-sender) err-invalid-percentage))
      (claimed-amount (get claimed-amount schedule))
      (claimable-amount (- vested-amount claimed-amount))
    )
    (asserts! (>= stacks-block-height (get start-block schedule)) err-vesting-not-started)
    (asserts! (> claimable-amount u0) err-no-vested-amount)
    (map-set vesting-schedules
      { nft-id: nft-id, collaborator: tx-sender }
      (merge schedule { claimed-amount: vested-amount })
    )
    (try! (as-contract (stx-transfer? claimable-amount tx-sender tx-sender)))
    (ok claimable-amount)
  )
)

(define-read-only (get-vesting-info (nft-id uint) (collaborator principal))
  (match (map-get? vesting-schedules { nft-id: nft-id, collaborator: collaborator })
    some-schedule 
      (let
        (
          (vested-result (calculate-vested-amount nft-id collaborator))
          (vested-amount (match vested-result ok-val ok-val err-val u0))
          (claimed-amount (get claimed-amount some-schedule))
        )
        (ok {
          total-amount: (get total-amount some-schedule),
          start-block: (get start-block some-schedule),
          duration-blocks: (get duration-blocks some-schedule),
          vested-amount: vested-amount,
          claimed-amount: claimed-amount,
          claimable-amount: (- vested-amount claimed-amount)
        })
      )
    err-not-found
  )
)

(define-constant err-offer-exists (err u106))
(define-constant err-offer-expired (err u107))
(define-constant err-insufficient-offer (err u108))
(define-constant err-milestone-not-reached (err u109))
(define-constant err-invalid-milestone (err u110))
(define-constant err-insufficient-balance (err u111))
(define-constant err-zero-amount (err u112))

(define-map nft-offers
  { nft-id: uint, offer-id: uint }
  {
    offerer: principal,
    amount: uint,
    expiry-block: uint,
    active: bool
  }
)

(define-data-var offer-counter uint u0)

(define-public (make-offer (nft-id uint) (amount uint) (duration-blocks uint))
  (let
    (
      (offer-id (+ (var-get offer-counter) u1))
      (expiry-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (> amount u0) err-invalid-percentage)
    (asserts! (> duration-blocks u0) err-invalid-percentage)
    (asserts! (is-some (map-get? nft-metadata { nft-id: nft-id })) err-not-found)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set nft-offers
      { nft-id: nft-id, offer-id: offer-id }
      {
        offerer: tx-sender,
        amount: amount,
        expiry-block: expiry-block,
        active: true
      }
    )
    (var-set offer-counter offer-id)
    (ok offer-id)
  )
)

(define-public (accept-offer (nft-id uint) (offer-id uint))
  (let
    (
      (offer (unwrap! (map-get? nft-offers { nft-id: nft-id, offer-id: offer-id }) err-not-found))
      (nft-owner (unwrap! (nft-get-owner? collaborative-nft nft-id) err-not-found))
      (offerer (get offerer offer))
      (amount (get amount offer))
      (dynamic-royalty-rate (unwrap! (get-current-royalty-rate nft-id) err-invalid-percentage))
      (royalty-rate dynamic-royalty-rate)
      (royalty-amount (/ (* amount royalty-rate) u100))
      (seller-amount (- amount royalty-amount))
    )
    (asserts! (is-eq tx-sender nft-owner) err-owner-only)
    (asserts! (get active offer) err-not-found)
    (asserts! (< stacks-block-height (get expiry-block offer)) err-offer-expired)
    (initialize-sales-performance nft-id)
    (try! (as-contract (stx-transfer? seller-amount tx-sender nft-owner)))
    (if (> royalty-amount u0)
      (try! (as-contract (distribute-royalty nft-id royalty-amount)))
      true
    )
    (try! (nft-transfer? collaborative-nft nft-id nft-owner offerer))
    (update-sales-performance nft-id amount)
    (map-set nft-offers
      { nft-id: nft-id, offer-id: offer-id }
      (merge offer { active: false })
    )
    (ok true)
  )
)

(define-public (cancel-offer (nft-id uint) (offer-id uint))
  (let
    (
      (offer (unwrap! (map-get? nft-offers { nft-id: nft-id, offer-id: offer-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get offerer offer)) err-owner-only)
    (asserts! (get active offer) err-not-found)
    (try! (as-contract (stx-transfer? (get amount offer) tx-sender (get offerer offer))))
    (map-set nft-offers
      { nft-id: nft-id, offer-id: offer-id }
      (merge offer { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-offer (nft-id uint) (offer-id uint))
  (map-get? nft-offers { nft-id: nft-id, offer-id: offer-id })
)

(define-read-only (is-offer-valid (nft-id uint) (offer-id uint))
  (match (map-get? nft-offers { nft-id: nft-id, offer-id: offer-id })
    some-offer 
      (and 
        (get active some-offer)
        (< stacks-block-height (get expiry-block some-offer))
      )
    false
  )
)

(define-map sales-performance
  { nft-id: uint }
  {
    total-sales: uint,
    total-volume: uint,
    current-royalty-rate: uint
  }
)

(define-map royalty-milestones
  { nft-id: uint, milestone-id: uint }
  {
    sales-threshold: uint,
    volume-threshold: uint,
    new-royalty-rate: uint,
    activated: bool
  }
)

(define-data-var milestone-counter uint u0)

(define-public (set-royalty-milestone (nft-id uint) (sales-threshold uint) (volume-threshold uint) (new-royalty-rate uint))
  (let
    (
      (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
      (milestone-id (+ (var-get milestone-counter) u1))
    )
    (asserts! (is-eq tx-sender (get creator metadata)) err-owner-only)
    (asserts! (<= new-royalty-rate u30) err-invalid-percentage)
    (asserts! (or (> sales-threshold u0) (> volume-threshold u0)) err-invalid-milestone)
    (map-set royalty-milestones
      { nft-id: nft-id, milestone-id: milestone-id }
      {
        sales-threshold: sales-threshold,
        volume-threshold: volume-threshold,
        new-royalty-rate: new-royalty-rate,
        activated: false
      }
    )
    (var-set milestone-counter milestone-id)
    (ok milestone-id)
  )
)

(define-private (initialize-sales-performance (nft-id uint))
  (if (is-none (map-get? sales-performance { nft-id: nft-id }))
    (map-set sales-performance
      { nft-id: nft-id }
      {
        total-sales: u0,
        total-volume: u0,
        current-royalty-rate: u10
      }
    )
    false
  )
)

(define-private (update-sales-performance (nft-id uint) (sale-price uint))
  (let
    (
      (performance (default-to 
        { total-sales: u0, total-volume: u0, current-royalty-rate: u10 }
        (map-get? sales-performance { nft-id: nft-id })
      ))
      (new-total-sales (+ (get total-sales performance) u1))
      (new-total-volume (+ (get total-volume performance) sale-price))
    )
    (map-set sales-performance
      { nft-id: nft-id }
      {
        total-sales: new-total-sales,
        total-volume: new-total-volume,
        current-royalty-rate: (get current-royalty-rate performance)
      }
    )
    true
  )
)

(define-private (check-and-activate-milestones (nft-id uint) (milestone-id uint))
  (let
    (
      (milestone (map-get? royalty-milestones { nft-id: nft-id, milestone-id: milestone-id }))
      (performance (map-get? sales-performance { nft-id: nft-id }))
    )
    (match milestone
      some-milestone
        (match performance
          some-performance
            (if (and 
                  (not (get activated some-milestone))
                  (or
                    (and (> (get sales-threshold some-milestone) u0) (>= (get total-sales some-performance) (get sales-threshold some-milestone)))
                    (and (> (get volume-threshold some-milestone) u0) (>= (get total-volume some-performance) (get volume-threshold some-milestone)))
                  )
                )
              (begin
                (map-set royalty-milestones
                  { nft-id: nft-id, milestone-id: milestone-id }
                  (merge some-milestone { activated: true })
                )
                (map-set sales-performance
                  { nft-id: nft-id }
                  (merge some-performance { current-royalty-rate: (get new-royalty-rate some-milestone) })
                )
                true
              )
              false
            )
          false
        )
      false
    )
  )
)

(define-public (trigger-milestone-check (nft-id uint) (milestone-id uint))
  (let
    (
      (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
      (milestone (unwrap! (map-get? royalty-milestones { nft-id: nft-id, milestone-id: milestone-id }) err-not-found))
    )
    (asserts! (not (get activated milestone)) err-already-exists)
    (if (check-and-activate-milestones nft-id milestone-id)
      (ok true)
      err-milestone-not-reached
    )
  )
)

(define-read-only (get-sales-performance (nft-id uint))
  (map-get? sales-performance { nft-id: nft-id })
)

(define-read-only (get-royalty-milestone (nft-id uint) (milestone-id uint))
  (map-get? royalty-milestones { nft-id: nft-id, milestone-id: milestone-id })
)

(define-read-only (get-current-royalty-rate (nft-id uint))
  (match (map-get? sales-performance { nft-id: nft-id })
    some-performance (ok (get current-royalty-rate some-performance))
    (ok u10)
  )
)

(define-map withdrawal-pool
  { collaborator: principal }
  { pending-balance: uint }
)

(define-public (deposit-to-pool (collaborator principal) (amount uint))
  (let
    (
      (current-balance (default-to 
        { pending-balance: u0 }
        (map-get? withdrawal-pool { collaborator: collaborator })
      ))
      (new-balance (+ (get pending-balance current-balance) amount))
    )
    (asserts! (> amount u0) err-zero-amount)
    (map-set withdrawal-pool
      { collaborator: collaborator }
      { pending-balance: new-balance }
    )
    (ok true)
  )
)

(define-public (withdraw-from-pool)
  (let
    (
      (balance-entry (unwrap! (map-get? withdrawal-pool { collaborator: tx-sender }) err-not-found))
      (amount (get pending-balance balance-entry))
    )
    (asserts! (> amount u0) err-insufficient-balance)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set withdrawal-pool
      { collaborator: tx-sender }
      { pending-balance: u0 }
    )
    (ok amount)
  )
)

(define-read-only (get-pending-withdrawal (collaborator principal))
  (match (map-get? withdrawal-pool { collaborator: collaborator })
    some-entry (ok (get pending-balance some-entry))
    (ok u0)
  )
)

(define-private (distribute-to-pool (nft-id uint) (collaborator principal) (amount uint))
  (if (> amount u0)
    (match (deposit-to-pool collaborator amount)
      success true
      error false
    )
    false
  )
)
