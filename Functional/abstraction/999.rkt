#lang racket
(require 2htdp/universe 2htdp/batch-io test-engine/racket-tests racket/match 2htdp/image)

;; A Nonary is a natural number in [1, 9]
;; data interpretation a nonary either represents
;; a particpant in the Nonary Game's bracelet number
;; or a door in the Nonary game for which
;; entry requires 3-5 participants bracelets's digital root
;; to line up with the door number

;; (List Nonary) -> Nonary
;; Calculates the digital root for the participants'
;; bracelet numbers
(define (digital-root bracelets)
  (let ([sum (apply + bracelets)])
  (+ (quotient sum 10) (remainder sum 10))))

;; Nonary (List Nonary) Nonary -> Boolean
(define (could-kill? victim possible-killers door)
  (= (digital-root (cons victim possible-killers)) door))


;; Nonary Nonary -> (List Nonary)
;; Determine what combinations of two people could have
;; pushed a victim through the door
(define (2-killers victim door)
  (define (two-k-help bracelet-1 bracelet-2 answers)
    (cond
      [(zero? bracelet-1) answers]
      [(zero? bracelet-2) (two-k-help (sub1 bracelet-1) 9 answers)]
      [(or (= bracelet-1 bracelet-2) (= victim bracelet-1) (= victim bracelet-2))
       (two-k-help bracelet-1 (sub1 bracelet-2) answers)]
      [(member (sort (list bracelet-1 bracelet-2 victim) <) answers)
       (two-k-help bracelet-1 (sub1 bracelet-2) answers)]
      [(could-kill? victim (list bracelet-1 bracelet-2) door)
       (two-k-help
        bracelet-1
        (sub1 bracelet-2)
        (cons (sort (list bracelet-1 bracelet-2 victim) <) answers))]
      [else (two-k-help bracelet-1 (sub1 bracelet-2) answers)]))
  (two-k-help 9 9 '()))

;; Nonary Nonary -> (List Nonary)
;; Determine what combination of three people could have
;; pushed a victim through the door
(define (3-killers victim door)
  (define (k-help killers-list answers)
    (match killers-list
      [(vector 0 y z) answers]
      [(vector x 0 z) (k-help (vector (sub1 x) 9 z) answers)]
      [(vector x y 0) (k-help (vector x (sub1 y) 9) answers)]
      [(vector x x z) (k-help (vector x x (sub1 z)) answers)]
      [(vector x y x) (k-help (vector x y (sub1 x)) answers)]
      [(vector x y y) (k-help (vector x y (sub1 y)) answers)]
      [(vector x y z) #:when (or
                              (member victim (list x y z))
                              (member (sort (list victim x y z) <) answers))
                      (k-help (vector x y (sub1 z)) answers)]
      [(vector x y z) #:when (could-kill? victim (list x y z) door)
                      (define possible-k (sort (list x y z victim) <))
                      (k-help (vector x y (sub1 z)) (cons possible-k answers))]
      [(vector x y z) (k-help (vector x y (sub1 z)) answers)]))
  (k-help (vector 9 9 9) '()))

;; Nonary Nonary -> (List Nonary)
;; Determine what combination of four people could have
;; pushed a victim through the door
(define (4-killers victim door)
  (define (k-help killers-list answers)
    (match killers-list
      [(vector 0 x y z) answers]
      [(vector v 0 y z) (k-help (vector (sub1 v) 0 y z) answers)]
      [(vector v x 0 z) (k-help (vector v (sub1 x) 9 z) answers)]
      [(vector v x y 0) (k-help (vector v x (sub1 y) 9) answers)]
      [(vector v x y z)
       (cond
         [(or
           (member v (list x y z))
           (member x (list v y z))
           (member y (list v x z))
           (member z (list v x y))
           (member victim (list v x y z)))
          (k-help (vector v x y (sub1 z)) answers)]
         [(member (sort (list victim v x y z) <) answers) (k-help (vector v x y (sub1 z)) answers)]
         [(could-kill? victim (list v x y z) door)
          (define possible-k (sort (list v x y z victim) <))
          (k-help (vector v x y (sub1 z)) (cons possible-k answers))]
         [else (k-help (vector v x y (sub1 z)) answers)])]))
  (k-help (vector 9 9 9 9) '()))

(define (list< lst1 lst2)
  (cond
    [(< (length lst1) (length lst2)) #t]
    [(> (length lst1) (length lst2)) #f]
    [(and (empty? lst1) (empty? lst2)) #t]
    [(< (first lst1) (first lst2)) #t]
    [(= (first lst1) (first lst2)) (list< (rest lst1) (rest lst2))]
    [else #f]))
        
                  

;; 
(define (possible-killers victim door)
  (sort (append (2-killers victim door) (3-killers victim door) (4-killers victim door)) list<))

(define (generate-line killers)
  (foldr (lambda (x y) (string-append (number->string x) " " y)) "\n" killers))

(define (generate-file k-list)
  (apply string-append (map generate-line k-list)))

;;(write-file "killers.txt" (generate-file (possible-killers 2 3)))
(define (main)
  (fprintf (current-output-port) "Enter the number of the victim: "  )
  (define victim-string (read-line (current-input-port) 'any))
  (fprintf (current-output-port) "Enter the door they were killed in: ")
  (define door-string (read-line (current-input-port) 'any))
  (define expected-output
    (possible-killers (string->number victim-string) (string->number door-string)))
  (fprintf (current-output-port) "What is your output file? ")
  (define in-file (read-line (current-input-port) 'any))
  (define parsed-answer (read-words/line in-file))
  (if (equal?
       (map (lambda (l) (map string->number l)) parsed-answer)
       expected-output)
      (match (system-type 'os) 
         ['windows (process "explorer.exe good-end.png")]
         ['unix (process "xdg-open good-end.png")]
         ['macosx (process "open good-end.png")])
      (match (system-type 'os) 
         ['windows (process "explorer.exe axe-end.png")]
         ['unix (process "xdg-open axe-end.png")]
         ['macosx (process "open axe-end.png")])))

(main)


;;(bitmap "good-end.png")
;;(bitmap "axe-end.png")
