(impl-trait 'ST1AE8AYE8GCXVX4711Y9B8D7BKVTYFYQTDKJJ3JR.nft-trait.nft-trait)
(use-trait commission-trait .commission-trait.commission)

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
(define-constant ERR-AFTER-MINT-TIME (err u510))

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
(define-map mint-address bool principal)

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

;; SIP009: Get the last token ID
(define-read-only (get-last-token-id)
  (if (< (var-get last-id) STX-MINT-LIMIT)
    (ok (var-get last-id))
  )
)

;; SIP009: Get the token URI. You can set it to any other URI
(define-read-only (get-token-uri (token-id uint))
  (if (< token-id u5001)
    (ok (some (concat (concat (var-get base-uri) (unwrap-panic (contract-call? .conversion lookup token-id))) ".json")))
    (ok (some (concat (concat (var-get base-uri) (unwrap-panic (contract-call? .conversion-v2 lookup (- token-id u5001)))) ".json")))
    )
)

;; SIP009: Get the token URI. You can set it to any other URI
(define-read-only (get-total-royalty)
  (ok (+ (+ (var-get royalty-1) (var-get royalty-2)) (var-get royalty-3)))
)

(define-read-only (get-last-id)
    (ok (var-get last-id))
)

(define-read-only (get-contract-uri)
  (ok (var-get contract-uri)))

;; Mint - pre / public first / public second
(define-public (pre-mint (new-owner principal) (mint-amount uint))
    (let (
        (cost-per-mint (var-get stx-cost-per-pre-mint))
        (mint-limit (var-get pre-mint-limit))
      )
      (asserts! (is-pre-mint) ERR-NOT-AUTHORIZED)
      (begin
        (mint principal cost-per-mint mint-amount mint-limit))
    )
)

(define-public (public-first-mint (new-owner principal))
    (let (
        (cost-per-mint (var-get stx-cost-per-public-first-mint))
        (mint-limit (var-get public-first-mint-limit))
      )
      (asserts! (is-pre-mint) ERR-NOT-AUTHORIZED)
      (begin
        (mint principal cost-per-mint mint-amount mint-limit))
    )
)

(define-public (public-second-mint (new-owner principal))
    (let (
        (cost-per-mint (var-get stx-cost-per-public-second-mint))
        (mint-limit (var-get public-second-mint-limit))
      )
      (asserts! (is-pre-mint) ERR-NOT-AUTHORIZED)
      (begin
        (mint principal cost-per-mint mint-amount mint-limit))
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
  (let ((owner (unwrap! (nft-get-owner? citycats id) false)))
    (or (is-eq tx-sender owner) (is-eq contract-caller owner))))


;; Set base uri
(define-public (set-base-uri (new-base-uri (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set base-uri new-base-uri)
    (ok true)))

;; Set contract uri
(define-public (set-contract-uri (new-contract-uri (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-uri new-contract-uri)
    (ok true))
)

;; Manage the Mint
(define-private (is-pre-mint)
  (let ((the-mint
          (unwrap! (map-get? mint-address true)
                    false)))
    (is-eq contract-caller the-mint)))

;; can only be called once
(define-public (set-mint-address)
  (let ((the-mint (map-get? mint-address true)))
    (asserts! (and (is-none the-mint)
              (map-insert mint-address true tx-sender))
                ERR-MINT-ALREADY-SET)
    (ok tx-sender)))

;; set for stx cost per mint
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
    (var-set stx-stx-cost-per-public-second-mint amount)
    (ok true)))