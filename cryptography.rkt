;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cryptography) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (encrypt m e n) calculates chiphertext c through the use of public key n,
;;    plaintext m, and public exponent e
;; Example:
(check-expect (encrypt 3 7 247) 211)

(define (encrypt m e n)
    (modulo (expt m e) n))

;; (decrypt c p q e) calculates plaintext m through the use of primes p, q,
;;    ciphertext c, and public exponent e
;; Example:
(check-expect (decrypt 211 19 13 7) 3)

(define (decrypt c p q e)
  (local [(define (decrypt/a c d n)
            (modulo (expt c d) n))
          (define (n-calc p q)
            (* p q))
          (define (d-calc p q e)
            (local [(define (d-calc/aa p q e k-now)
                      (local [(define (d-calc/a p q e k)
                                (local [(define phi (* (- p 1) (- q 1)))]
                                  (/ (add1 (* k phi)) e)))]
                        (cond
                          [(integer? (d-calc/a p q e k-now))
                           (d-calc/a p q e k-now)]
                          [else (d-calc/aa p q e (add1 k-now))])))]
              (d-calc/aa p q e 1)))]
    (decrypt/a c (d-calc p q e) (n-calc p q))))