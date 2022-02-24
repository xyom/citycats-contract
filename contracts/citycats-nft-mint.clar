;; Storage
(define-map presale-count principal uint)

;; Define Constants
(define-constant pre-sale-mint-price u45000000) ;; 45 STX
(define-constant public-sale-mint-price u50000000) ;; 50 STX

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-SALE-NOT-ACTIVE (err u500))
(define-constant ERR-NO-PRE-SALE-REMAINING (err u501))

;; Define Variables
(define-data-var pre-sale-active bool true)
(define-data-var public-sale-active bool false)

;; Presale balance
(define-read-only (get-presale-balance (account principal))
  (default-to u0
    (map-get? presale-count account)))

;; Mint a new CityCats NFT
(define-public (mint)
  (if (var-get pre-sale-active)
    (pre-mint tx-sender)
    (public-mint tx-sender)))

(define-public (mint-two)
  (begin
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-three)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-four)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

(define-public (mint-five)
  (begin
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (try! (mint))
    (ok true)))

;; Internal - Mint NFT using pre sale mechanism
(define-private (pre-mint (new-owner principal))
  (let ((presale-balance (get-presale-balance new-owner)))
    (asserts! (> presale-balance u0) ERR-NO-PRE-SALE-REMAINING)
    (map-set presale-count
              new-owner
              (- presale-balance u1))
  (contract-call? .citycats-nft mint new-owner pre-sale-mint-price)))

;; Internal - Mint public sale NFT
(define-private (public-mint (new-owner principal))
  (begin
    (asserts! (var-get public-sale-active) ERR-SALE-NOT-ACTIVE)
    (contract-call? .citycats-nft mint new-owner public-sale-mint-price)))

;; Set public sale flag
(define-public (flip-pre-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Disable the Public sale
    (var-set public-sale-active false)
    (var-set pre-sale-active (not (var-get pre-sale-active)))
    (ok (var-get pre-sale-active))))

;; Set public sale flag
(define-public (flip-sale)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Disable the public sale
    (var-set pre-sale-active false)
    (var-set public-sale-active (not (var-get public-sale-active)))
    (ok (var-get public-sale-active))))

(as-contract (contract-call? .citycats-nft set-mint-address))

;; Pre Mint Addresses