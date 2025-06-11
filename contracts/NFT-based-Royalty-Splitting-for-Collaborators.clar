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

  (define-public (distribute-royalty (nft-id uint) (amount uint))
    (let
      (
        (metadata (unwrap! (map-get? nft-metadata { nft-id: nft-id }) err-not-found))
      )
      (asserts! (is-eq (get total-shares metadata) u100) err-invalid-percentage)
      (ok true)
    )
  )

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