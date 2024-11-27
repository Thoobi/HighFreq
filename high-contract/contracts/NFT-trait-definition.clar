
;; title: NFT-trait-definition
;; version:
;; summary:
;; description:
;; SIP-009: NFT trait definition

(define-trait nft-trait
    (
        ;; Last token ID, limited to uint range
        (get-last-token () (response uint uint))

        ;; URI for metadata associated with the token 
        (get-token-uri (uint) (response (optional (string-ascii 256)) uint))

        ;; Owner of a token
        (get-owner (uint) (response (optional principal) uint))

        ;; Transfer from one principal to another
        (transfer (uint principal principal) (response bool uint))
    )
)