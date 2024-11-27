;; Import NFT trait
(use-trait nft-trait .NFT-trait-definition.nft-trait)

;; Define constants for subscription tiers
(define-constant ERR-EXPIRED (err u1))
(define-constant ERR-INVALID-TIER (err u2))
(define-constant ERR-UNAUTHORIZED (err u3))

;; Define contract owner
(define-data-var contract-owner principal tx-sender)

;; Define subscription tiers
(define-data-var BASIC-PRICE uint u100)
(define-data-var PRO-PRICE uint u250)
(define-data-var PREMIUM-PRICE uint u500)

;; Track token IDs
(define-data-var last-token-id uint u0)

;; NFT metadata storage
(define-map token-metadata
    uint
    {
        tier: (string-ascii 20),
        expiration: uint,
        creator: principal,
        content-uri: (string-ascii 256)
    })

;; NFT ownership
(define-map token-owner
    uint
    principal)

;; Check if caller is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner)))

;; Get token owner
(define-read-only (get-owner (token-id uint))
    (ok (map-get? token-owner token-id)))

;; Get last token ID
(define-read-only (get-last-token)
    (ok (var-get last-token-id)))

;; Get token metadata
(define-read-only (get-token-metadata (token-id uint))
    (ok (map-get? token-metadata token-id)))

;; Check if token is expired
(define-private (is-token-expired (token-id uint))
    (match (map-get? token-metadata token-id)
        metadata (let ((current-time block-height))
            (> current-time (get expiration metadata)))
        false))

;; Get tier price
(define-private (get-tier-price (tier (string-ascii 20)))
    (if (is-eq tier "basic")
        (var-get BASIC-PRICE)
        (if (is-eq tier "pro")
            (var-get PRO-PRICE)
            (if (is-eq tier "premium")
                (var-get PREMIUM-PRICE)
                u0))))

;; Validate tier
(define-private (is-valid-tier (tier (string-ascii 20)))
    (or
        (is-eq tier "basic")
        (is-eq tier "pro")
        (is-eq tier "premium")))

;; Mint new NFT
(define-public (mint-nft (tier (string-ascii 20)) (duration uint) (content-uri (string-ascii 256)))
    (let ((token-id (+ (var-get last-token-id) u1))
          (price (get-tier-price tier)))
        (asserts! (is-valid-tier tier) ERR-INVALID-TIER)
        (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
        
        ;; Update maps and variables
        (map-set token-metadata token-id
            {
                tier: tier,
                expiration: (+ block-height duration),
                creator: tx-sender,
                content-uri: content-uri
            })
        (map-set token-owner token-id tx-sender)
        (var-set last-token-id token-id)
        
        (ok token-id)))

;; Transfer NFT
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR-UNAUTHORIZED)
        (asserts! (not (is-token-expired token-id)) ERR-EXPIRED)
        
        (map-set token-owner token-id recipient)
        (ok true)))

;; Update content URI
(define-public (update-content-uri (token-id uint) (new-uri (string-ascii 256)))
    (let ((metadata (unwrap! (map-get? token-metadata token-id) ERR-UNAUTHORIZED)))
        (asserts! (is-eq tx-sender (get creator metadata)) ERR-UNAUTHORIZED)
        
        (map-set token-metadata token-id
            (merge metadata { content-uri: new-uri }))
        (ok true)))

;; Extend subscription
(define-public (extend-subscription (token-id uint) (duration uint))
    (let ((metadata (unwrap! (map-get? token-metadata token-id) ERR-UNAUTHORIZED))
          (price (get-tier-price (get tier metadata))))
        (asserts! (is-eq tx-sender (get creator metadata)) ERR-UNAUTHORIZED)
        (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
        
        (map-set token-metadata token-id
            (merge metadata { expiration: (+ (get expiration metadata) duration) }))
        (ok true)))

;; SIP-009 required functions
(define-public (get-token-uri (token-id uint))
    (ok (get content-uri (unwrap! (map-get? token-metadata token-id) ERR-UNAUTHORIZED))))

;; Function to transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)))