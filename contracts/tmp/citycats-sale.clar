(impl-trait .sip-009-trait-ft-standard.nft-trait)

(define-non-fungible-token citycats uint)

;; Storage
(define-map token-count principal uint)
(define-map minted principal bool)

;; Define Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant STX-MINT-LIMIT u2050)

(define-constant ERR-SOLD-OUT (err u300))
(define-constant ERR-WRONG-COMMISSION (err u301))
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-MINT-ALREADY-SET (err u506))
(define-constant ERR-LISTING (err u507))
(define-constant ERR-ONE-MINT-PER-WALLET (err u508))
(define-constant ERR-BEFORE-MINT-TIME (err u509))

;; Define Variables
(define-data-var last-id uint u0)

(define-data-var stx-cost-per-pre-mint uint u45000000) ;; Minting price: 45 STX
(define-data-var stx-cost-per-public-first-mint uint u45000000) ;; Minting price: 45 STX
(define-data-var stx-cost-per-public-second-mint uint u45000000) ;; Minting price: 45 STX

(define-data-var pre-mint-limit uint u100) 
(define-data-var public-first-mint-limit uint u1000)
(define-data-var public-second-mint-limit uint u1000)

(define-data-var pre-mint-block-height uint u1) 
(define-data-var public-first-mint-block-height uint u2)
(define-data-var public-second-mint-block-height uint u3)

(define-data-var base-uri (string-ascii 100) "ipfs://placeholder/")
(define-data-var contract-uri (string-ascii 100) "ipfs://placeholder")

(define-data-var treasure-address principal 'ST1AE8AYE8GCXVX4711Y9B8D7BKVTYFYQTDKJJ3JR)

(define-map pre-mint-whitelist-address principal bool)

;; Token count for account
(define-read-only (get-balance (account principal))
  (default-to u0
    (map-get? token-count account)))

;; Get minted
(define-read-only (get-minted (account principal))
  (default-to false
    (map-get? minted account)))

(define-private (trnsfr (id uint) (sender principal) (recipient principal))
  (match (nft-transfer? citycats id sender recipient)
        success
          (let
            ((sender-balance (get-balance sender))
            (recipient-balance (get-balance recipient)))
              (map-set token-count
                    sender
                    (- sender-balance u1))
              (map-set token-count
                    recipient
                    (+ recipient-balance u1))
              (ok success))
        error (err error)))

;; SIP009: Transfer token to a specified principal
(define-public (transfer (id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (trnsfr id sender recipient)))

;; SIP009: Get the owner of the specified token ID
(define-read-only (get-owner (id uint))
  ;; Make sure to replace citycats
  (ok (nft-get-owner? citycats id)))

;; SIP009: Get the token URI. You can set it to any other URI
(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat (concat (var-get base-uri) (unwrap-panic (contract-call? .index-conversion lookup token-id))) ".json")))
)

;; SIP009: Get the last token ID
(define-read-only (get-last-token-id)
    (ok (var-get last-id))
)

(define-read-only (get-contract-uri)
  (ok (var-get contract-uri)))

;; MINT: sales for pre and public first and public second
(define-public (pre-mint (new-owner principal) (mint-amount uint))
    (let (
        (cost-per-mint (var-get stx-cost-per-pre-mint))
        (mint-limit (var-get pre-mint-limit))
        (is-mint-owner (unwrap-panic (map-get? pre-mint-whitelist-address new-owner)))
      )
      (asserts! is-mint-owner ERR-NOT-AUTHORIZED)
      (asserts! (>= (var-get pre-mint-block-height) block-height) ERR-BEFORE-MINT-TIME)
      (begin
        (mint new-owner cost-per-mint mint-amount mint-limit))
    )
)

(define-public (public-first-mint (new-owner principal) (mint-amount uint))
    (let (
        (cost-per-mint (var-get stx-cost-per-public-first-mint))
        (mint-limit (var-get public-first-mint-limit))
      )
      (asserts! (>= (var-get public-first-mint-block-height) block-height) ERR-BEFORE-MINT-TIME)
      (begin
        (mint new-owner cost-per-mint mint-amount mint-limit))
    )
)

(define-public (public-second-mint (new-owner principal) (mint-amount uint))
    (let (
        (cost-per-mint (var-get stx-cost-per-public-second-mint))
        (mint-limit (var-get public-second-mint-limit))
      )
      (asserts! (>= (var-get public-second-mint-block-height) block-height) ERR-BEFORE-MINT-TIME)
      (begin
        (mint new-owner cost-per-mint mint-amount mint-limit))
    )
)

(define-private (mint (new-owner principal) (price uint) (mint-amount uint) (mint-limit uint))
    (let (
        (next-id (+ u1 (var-get last-id)))
      )
      (asserts! (< (var-get last-id) mint-limit) ERR-SOLD-OUT)
      (match (nft-mint? citycats next-id new-owner)
        success
        (let
        ((current-balance (get-balance new-owner)))
          (begin
            (print "mint in stx")
            (try! (stx-transfer? price tx-sender (var-get treasure-address)))
            (var-set last-id next-id)
            (map-set token-count
              new-owner
              (+ current-balance u1)
            )
            (map-set minted new-owner true)
            (ok true)))
        error (err (* error u10000)))))

;; Burn
(define-public (burn (id uint) (owner principal))
    (let (
        (token-owner (unwrap-panic (unwrap-panic (get-owner id))))
    )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq tx-sender token-owner) ERR-NOT-AUTHORIZED)
    (match (nft-burn? citycats id owner)
        success
        (let
        ((current-balance (get-balance owner)))
          (begin
            (map-set token-count
              owner
              (- current-balance u1)
            )
            (ok true)))
        error (err (* error u10000)))
    )
)

;; Check if it's owner
(define-private (is-owner (id uint))
  (let (
    (owner (unwrap! (nft-get-owner? citycats id) false))
    )
    (or (is-eq tx-sender owner) (is-eq contract-caller owner))
  )
)

;; Set base uri
(define-public (set-base-uri (new-base-uri (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set base-uri new-base-uri)
    (ok true)
  )
)

;; Set contract uri
(define-public (set-contract-uri (new-contract-uri (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-uri new-contract-uri)
    (ok true)
  )
)

;; Set whitelist for pre-minting
(define-public (set-pre-mint-whitelist-address)
  (begin
    (map-insert pre-mint-whitelist-address tx-sender true)
    (ok tx-sender)
    )
)

;; Set for stx cost per mint
(define-public (set-stx-cost-per-pre-mint (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set stx-cost-per-pre-mint amount)
    (ok true)))

(define-public (set-stx-cost-per-public-first-mint (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set stx-cost-per-public-first-mint amount)
    (ok true)))

(define-public (set-stx-cost-per-public-second-mint (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set stx-cost-per-public-second-mint amount)
    (ok true)))