;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ProjectEuler) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require math/number-theory) ;;the function factorize used in Problem 5 is from this library 

;;************************************
;; PROJECT EULER
;;************************************
;;(prime-theory n) consumes an integer number and determines whether it is prime or not.
;;   It uses the prime theory (Sieving Algorithm) that if a number n is not divisible by any numbers less
;;    than it's square root, it is prime.
;; prime-theory: Int -> Bool
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
;; (my-factorial n) produces the factorial of consumed natural number n
;; my-factorial: Nat -> Nat
(define (my-factorial n)
  (foldr (lambda (first rest) (* first rest)) 1 (build-list n add1)))
(check-expect (my-factorial 2) 2)
(check-expect (my-factorial 5) 120)
(check-expect (my-factorial 0) 1)

;; (my-prime? n) checks if n is a prime.
;; Using Wilson's Theorem: if n > 3 and n is a prime then (n - 1)! is congruent to -1 modulus n
;; Remember that the only 2 primes that Wilson's theorem does not hold true for is 2 and 3
;; my-prime?: Int -> Bool
(define (my-prime? n)
  (and (not (= n 0)) (not (= (abs n) 1)) (or (= (abs n) 2) (= (abs n) 3) 
                                             (= (remainder (my-factorial (- (abs n) 1)) (abs n)) (- (abs n) 1)))))
(check-expect (my-prime? 0) false)
(check-expect (my-prime? 1) false)
(check-expect (my-prime? 2) true)
(check-expect (my-prime? 3)true)
(check-expect (my-prime? 7) true)
(check-expect (my-prime? 66)false)
(check-expect (my-prime? 65)false)
(check-expect (my-prime? 91) false)
(check-expect (my-prime? 87) false)
(check-expect (my-prime? 103)true)
;;************************************
;; PROBLEM 1
;; (euler1 x) finds the sum of all the multiples of 3 or 5 below x.
;; using the fact that the sum of all natural numbers from 1 to n is given by
;;  n * (n + 1) / 2
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
(check-expect (= (local [(define phi (/ (+ 1 (sqrt 5)) 2))
                         (define phi- (/ (- 1 (sqrt 5) )2))
                         (define (find-nth n) (/ (- (expt phi n)(expt phi- n)) (sqrt 5)))]
                   (floor  (foldr + 0 (map find-nth (build-list 12 (lambda (x) (* 3 x))))))) 4613732) true)
;; Answer: 4613732
;;************************************
;; PROBLEM 5
;; (euler5 x) finds the smallest number that is evenly divisible by all numbers from 1..n
;; euler5: Nat -> Nat
;; requires n > 0
(define (euler5 x)
  (local [(define (highest-power listoffactors x)
            (cond
              [(empty? listoffactors) empty]
              [else
               (local [(define factor (filter (lambda (lst) (= x (first lst))) listoffactors))]
                 (cond 
                   [(empty? factor) (highest-power (rest listoffactors) (sub1 x))] 
                   [else (cons (first (sort factor (lambda (l1 l2) (> (second l1) (second l2)))))
                               (highest-power (filter (lambda (lst) (not (= x (first lst))))
                                                      listoffactors) (sub1 x)))]))]))]    
    (foldr (lambda (lst rest) (* rest (expt (first lst) (second lst))))
           1 (highest-power (foldr append empty
                                   (build-list (add1 x)
                                               (lambda (n) (factorize n))))
                            (add1 x)))))
(check-expect (euler5 20) 232792560)
(check-expect (euler5 10) 2520)  
;; ANSWER: 232792560
;;************************************
;; PROBLEM 6
;; (euler6 n) finds the difference between the sum of the squares of the first n number of natural
;;   numbers and the square of the sum of the first n number of natural numbers
;; euler6: Nat -> Nat
(define (euler6 n)
  (-
   (expt (/ (* n (+ 1 n)) 2) 2) ;formula for sum of first n
   (/ (* n (+ n 1) (+ 1 (* 2 n))) 6))) ;formula for sum of first n squares

(check-expect (euler6 10) 2640)
(check-expect (euler6 100) 25164150)
;; ANSWER: 25164150
;;************************************
;; PROBLEM 7
;; (euler7 n) produces the nth prime number 
;; euler7: Nat -> Nat
;; requires: n > 0
(define (euler7 n)
  (cond
    [(= 1 n) 2]
    [(even? n) (local
                 [(define primes
                    (append (list 2 3) (filter prime? (build-list n (lambda (x) (+ 1 (* 6 x)))))))]
                 (list-ref primes (floor (/ (length primes) 2))))]
    [(odd? n) (local
                 [(define primes
                    (filter prime? (build-list n (lambda (x) (- (* 6 x) 1)))))]
                 (list-ref primes (floor (/ (length primes) 2))))]))

(check-expect (euler7 6) 13)
(check-expect (euler7 7) 17)
(check-expect (euler7 8) 19)
(check-expect (euler7 9) 23)
(check-expect (euler7 2) 3)
(check-expect (euler7 1) 2)
(check-expect (euler7 1001) 7927)

;; ANSWER: 
;;************************************

;; PROBLEM 10
;; (euler10 n) finds the sum of all prime numbers less than consumed number n
;; euler10: Nat -> Nat
;; requires: n > 3
(define (euler10 n)
  (local [(define limit (ceiling (/ (- n 1) 6)))]
    (+ 5 (foldr + 0 (append (filter prime? (build-list limit (lambda (x) (+ 1 (* 6 x)))))
                            (filter prime? (build-list limit (lambda (x) (- (* 6 x) 1)))))))))
(check-expect (euler10 5) 5)
(check-expect (euler10 10) 17)
(check-expect (euler10 2000000) 142913828922)
;; ANSWER: 142913828922
;;************************************
;; PROBLEM 48
;; (euler48 n y) produces the last y digits of the sum of the self power series till the nth number
;; the self power series: 1^1 + 2^2 + 3^3 + 4^4...n^n
;; Nat Nat -> Nat
(define (euler48 n y)
  (remainder (- (foldr + 0 (build-list (add1 n) (lambda (n) (expt n n)))) 1) (expt 10 y)))

(check-expect (euler48 0 0)0)
(check-expect (euler48 0 9)0)
(check-expect (euler48 10 0)0)
(check-expect (euler48 10 11) 10405071317)
(check-expect (euler48 1000 10) 9110846700)
;; ANSWER: 9110846700

;; PROBLEM 16
;; if number is divisible by 4, last digit should be either 4 or 6
;; if expt is odd,  otherwise 6
;; so last 2 digits are 76
;; first digit is 1
;; (expt 2 1000)= 4^500 = 16^250 = 32^125
