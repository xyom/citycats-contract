;; constants
;;

(define-constant depositAmount u400000000)
(define-constant ERR-ALREADY-PARTICIPATED (err u5000))
(define-constant ERR-AMOUNT-EXCEED (err u5001))
(define-constant ERR-AMOUNT-SMALL (err u5002))

;; Storage
(define-map participant principal {number: (list 5 uint)})
(define-map ticket uint {winning: bool, owner: principal})

;; Define Variables
(define-data-var last-id uint u1)

(define-read-only (getLastId)
    (ok (var-get last-id))
)

(define-public (deposit (count uint))
    (begin
        ;; (asserts! (is-none (map-get? participant tx-sender)) ERR-ALREADY-PARTICIPATED)
        (asserts! (<= count u5) ERR-AMOUNT-EXCEED)
        (asserts! (> count u0) ERR-AMOUNT-SMALL)
        (let 
            (
                (totalDepositAmount (* depositAmount count))
                (currentId (var-get last-id))
                
            )
            (try! (stx-transfer? totalDepositAmount tx-sender (as-contract tx-sender)))

            (map-set participant tx-sender { number: (getTicketList currentId count)})
            (fold setTicketOwner (getTicketList currentId count) tx-sender)
            (var-set last-id (+ currentId count))
            (ok (map-get? participant tx-sender))
        )
    )
)

(define-read-only (getParticipant (address principal))
    (ok (map-get? participant address))
)

(define-private (getTicketList (lastNum uint) (count uint))
    (begin
        (if (is-eq count u1)
            (list lastNum)
            (if (is-eq count u2)
                (list lastNum (+ lastNum u1))
                (if (is-eq count u3)
                    (list lastNum (+ lastNum u1) (+ lastNum u2))
                    (if (is-eq count u4)
                        (list lastNum (+ lastNum u1) (+ lastNum u2) (+ lastNum u3))
                        (if (is-eq count u5)
                            (list lastNum (+ lastNum u1) (+ lastNum u2) (+ lastNum u3) (+ lastNum u4))
                            (list lastNum)
                        )
                    )
                )
            )   
        )
    )
)

(define-private (setTicketOwner (ticketNumber uint) (owner principal))
    (begin
        (map-set ticket ticketNumber {winning: false, owner: owner})
        owner
    )
)