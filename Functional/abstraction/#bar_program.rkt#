#lang racket
(struct person [name age] #:transparent)
(define SARA (person "Sara" 26))
(define DUSTIN (person "Dustin" 16))
(define PATRONS (list SARA DUSTIN))

;; List<person> -> List<person>
;; Add a person to the patron list as
;; long as they are over 18.
(define (cons-over-18 possible-patron patrons)
  (if (>= (person-age possible-patron) 18)
      (cons possible-patron patrons)
      patrons))

(define (bar-entry-18 patrons)
  (cond
    [(empty? patrons) patrons]
    [(cons? patrons) (cons-over-18
                      (first patrons)
                      (bar-entry-18 (rest patrons)))]))

(define (cons-over-21 possible-patron patrons)
  (if (>= (person-age possible-patron) 21)
      (cons possible-patron patrons)
      patrons))

(define (bar-entry-21 patrons)
  (cond
    [(empty? patrons) patrons]
    [(cons? patrons) (cons-over-21
                      (first patrons)
                      (bar-entry-21 (rest patrons)))]))



