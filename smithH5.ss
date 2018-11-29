;David Smith
;March 30
;description: this is scheme program that deals with diferent sorts such as quick and merge and other list operations
					;

					;This function will return the multiplicative reciprocal
					; of a numeric input value
					;This procedure, reciprocal, computes the quantity 1/n for any number .
					;For n = 0, reciprocal returns the string "oops!".
(define (reciprocal n)
  (if (= n 0)
      "oops!"
      (/ 1 n)
      )
  )


					;this s for sqare
(define (square n)
  (* n n))



					;15
					;Write and test a Scheme function called list_sumr that will return the sum of the numeric values in the
					;list. This function should use recursion to calculate the sum. The formal argument should be a list.
(define (list_sumr mylist)
  (if(null? mylist)
     0
     (+(car mylist) (list_sumr (cdr mylist)))
     )
  )



					;16
					;Write and test a recursive Scheme function called list_copy that will create a copy of a list. The
					;formal argument should be a list.

(define (list_copy mylist)
  (if(null? mylist)
     mylist
     (cons(car mylist) (list_copy (cdr mylist)
				  )
	  )
     )
  )

(define (odd_copy L)
  (if (null? L) '()
      (if (null? (cdr L)) (list (car L))
	  (cons (car L) (odd_copy (cddr L))))))

(define (even_copy L)
  (if (null? L) '()
      (if (null? (cdr L)) '()
	  (cons (cadr L) (even_copy (cddr L)
				    )
		)
	  )
      )
  )


					;19. The scheme command car returns the first value in a list. Write and test a Scheme function called
					;last that will return the last item in an existing list. The formal argument should be a list.
(define (last mylist)
  (if (null? (cdr mylist))
      (car mylist)
      (last (cdr mylist)
	    )
     )
  )


					;20. The scheme command cons inserts an item as the first item in an existing list. Write and test a
					;Scheme function called insert_last that will insert a value into a list as the last element of the list. The
					;formal arguments should be the value to be inserted into the list and the list.
(define (insert_last myvalue mylist)
  (if (null? mylist)
      (cons myvalue '())
      (cons (car mylist) (insert_last myvalue (cdr mylist)))
	    )
  )


					;21. The scheme command cdr returns a list after the first value has been removed. Write and test a
					;Scheme function called remove _last that will remove the last element of a list and return the
					;resulting list. The formal argument should be a list.
					;(reverse L) reverse the list L
(define (remove_last mylist)
  (if(null? (cdr mylist))
	    '()
	    (cons(car mylist) (remove_last(cdr mylist))))
	    
  )


;previously list_reverse ; now is the deep reverse					
(define (atom? x) ;checks if it is an atom
  (not (or (pair? x) (null? x))))

(define (all_reverse mylist)
  (if (null? mylist) ; base case
      '()
      (if (not(list? mylist))
	  "all_reverse requires a list argument"
	  (if (atom? (car mylist))
	      (append (all_reverse (cdr mylist))(cons (car mylist) '()))
	      (append (all_reverse (cdr mylist))(cons (all_reverse(car mylist)) '()))
	      )
	  )
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;
;merge sort

(define (merge_h L M) ;sorts a list using odd_copy and even_copy
  (if (null? L) M
      (if (null? M) L
	  (if (number? (car L)) 
	      (if (number? (car M)) 
		  (if (> (car L) (car M))
		      (cons (car L) (merge_h (cdr L) M))
		      (cons (car M) (merge_h (cdr M) L))
		      )
		  (merge_h L (cdr L))
		  )
	      (merge_h (cdr L) M)
	      )
	  )
      )
  )

(define (split L)
  (cons (odd_copy L) (cons (even_copy L) `())))


(define (mergesort L)
  (if (null? L)
      L
      (if (not (list? L))
	  "mergesort requires a list argument"
	  (if (null? (cdr L))
	      L
	      ;(if (and (number? (car L)) (number? (cadr L)))
		  (merge_h (mergesort (car (split L))) (mergesort (cadr (split L))))
		  ;(mergesort (cdr L))
		  ;)
		  )
	  )
      )
  )



;quicksort
(define (quicksort lst) ;sorts a list
  (if(not(list? lst)) ;base case
     "quicksort requires a list argument"
     (if(null? lst) ;base case
	'()
	(if(number? (car lst))
	   (combine(quicksort(less_than(car lst)(cdr lst))) ;combines all three of are functions recursively 
		   (equal(car lst)(cdr lst))
		   (quicksort(greater_than(car lst)(cdr lst))))
	(quicksort (cdr lst))))))
  

(define (less_than P L1)
  (if(null? L1) ;base case
     '()
     (if(number? (car L1))
	(if (< (car L1) P)
	    (append (cons(car L1) '())(less_than P (cdr L1)))  ; put everything into a list thats less than the pivot
	    (less_than P (cdr L1)))
	    (less_than P (cdr L1)))))
	 
(define (greater_than P L1)
  (if(null? L1) ;base case
     '()
     (if(number? (car L1))
	(if ( > (car L1) P)
	    (append (cons(car L1)'())(greater_than P (cdr L1))) ;puts everything into a list thats greater than the pivot
	    (greater_than P (cdr L1)))
	    (greater_than P (cdr L1)))))
	 

(define (equal P L1)
  (if(null? L1)
    (cons P '())
    (if(number? (car L1))
       (if ( = (car L1) P)
	   (append (cons(car L1)'())(equal P (cdr L1))) ;puts all things equal to the pivot in a list as well as the pivot ;not recusive
	   (equal P (cdr L1)))
	   (equal P (cdr L1)))))

(define(combine L1 L2 L3) ;combines 3 lists together using append
  (append(append L1 L2) L3))


