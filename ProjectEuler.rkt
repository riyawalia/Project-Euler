;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProjectEuler) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;************************************
;; PROJECT EULER
;;************************************
;;(prime-theory n) consumes an integer number and determines whether it is prime or not.
;;   It uses the prime theory (Sieving Algorithm) that if a number n is not divisible by any numbers less
;;    than it's square root, it is prime.
;; prime-theory: Int-> Bool
(define (prime-theory n)
  (and (not (= 1 n)) (not (ormap
                           (lambda (f)  (= 0 (remainder n f)))
                           (rest (build-list (integer-sqrt n) add1))))))

(check-expect (prime-theory 1) false)
(check-expect (prime-theory 2) true)
(check-expect (prime-theory 3)true)
(check-expect (prime-theory 4)false)
(check-expect (prime-theory 91)false)
(check-expect (prime-theory 79)true)
(check-expect (prime-theory 15485867) true)
;; (factorial n) produces the factorial of consumed natural number n
;; factorial: Nat-> Nat
(define (factorial n)
  (foldr (lambda (first rest) (* first rest)) 1 (build-list n add1)))
(check-expect (factorial 2) 2)
(check-expect (factorial 5) 120)
(check-expect (factorial 0) 1)

;; (prime? n) checks if n is a prime.
;; Using Wilson's Theorem
;; prime?: Nat-> Nat
;; requires: n>0
(define (prime? n)
  (and (not (= n 1)) (or (= n 2) (= n 3) 
                         (= (remainder (factorial (- n 1)) n) (- n 1)))))
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)
(check-expect (prime? 3)true)
(check-expect (prime? 7) true)
(check-expect (prime? 66)false)
(check-expect (prime? 65)false)
(check-expect (prime? 91) false)
(check-expect (prime? 87) false)
(check-expect (prime? 103)true)
;;************************************
;; PROBLEM 1
;; (euler1 n) finds the sum of all the multiples of 3 or 5 below n.
;; using the fact that the sum of all natural numbers from 1 to n is given by
;;  n(n+1)/2
;; requires: x>0
;; euler1: Num-> Nat
(define (euler1 x)
  (local [(define n (- x 1))
          (define multiples3 (floor (/ n 3)))
          (define multiples5 (floor (/ n 5)))
          (define multiples15 (floor (/ n 15)))
          (define (sum n) (/ (* n (+ n 1)) 2))]
    (- (+ ( * 3 (sum multiples3)) (* 5 (sum multiples5))) (* 15 (sum multiples15)))))

(check-expect (euler1 1000) 233168)
(check-expect (euler1 10)23)
;; Answer: 233168
;;************************************
;; PROBLEM 2
;; Find the sum of the even-valued terms in the Fibonacci sequence that do
;;  not exceed 4 million
(local [(define phi (/ (+ 1 (sqrt 5)) 2))
        (define phi- (/ (- 1 (sqrt 5) )2))
        (define (find-nth n) (/ (- (expt phi n)(expt phi- n)) (sqrt 5)))]
  (floor  (foldr + 0 (map find-nth (build-list 12 (lambda (x) (* 3 x)))))))
;; Answer: 4613732
;;************************************