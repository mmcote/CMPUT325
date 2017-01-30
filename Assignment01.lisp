;QUESTION 1\
; Create the lisp function:
; - returns true if argument Y is a member of the argument list X
; - This should also test for lists being members of lists
; - The argument Y may be nil or a list containing nil

(defun xmember (X Y)
	; check if X is even a list --VOID not allowed to use listp
	
	; check if the list is empty
	(if (null X)
		; if the list is empty it clearly is not in the list
		NIL
		; check if the first element is equal to the requested item
		(if (equal (car X) Y)
			; if so return true
			T
			; else call function again to compare next element
			(xmember (cdr X) Y)
		)
	)
)

;QUESTION 2
; Create a lisp function:
; - x is a list with sublists nested to any depth
; - the returned value is a list of all the atom elements
; - with the property that all atoms appearing in x also appear in the same order
; - NIL will not appear

; ASSUMPTIONS
; - ONLY ATOMS OR LISTS
(defun flatten (x)
	; check if the list is now empty
	(cond ((null x) NIL)
		; take the first item and check if atom, append to return list if atom
		((atom (car x)) (append (list (car x)) (flatten (cdr x))))
		; if not atom we assume it is a list and should explore
		((not (atom (car x))) (append (flatten (car x)) (flatten (cdr x))))
	)
)


;QUESTION 3
; Create a lisp function:
; - mixes the elements fo two lists L1 and L2
; - returning a single list

; This function will handle one list at a time and alternate them on each recursive call
(defun mix (L1 L2)
	; check if either of the list are empty
	(cond ((null L2) L1)
		; add one element of L1 to the mixed list
		(T (append (list (car L2)) (mix (cdr L2) L1)))
	)
)

;QUESTION 4
; Create a lisp function:
; - 	(if (not (null (cdr L)))
(defun split (L)
	(cond ((null L) (NIL NIL))
		((null (cddr L)) (append (list (list (car L))) (list (cdr L))))
		(T (let ((tuple (split (cddr L))))
				(append (list (append (list (car L)) (car tuple))) (list (append (list (cadr L)) (cadr tuple))))
			))
	)
)

;QUESTION 6
(defun subset-sum-check (S L)	
	(cond 
		; first condition is if the sum requested is zero then we can just return an empty list
		((= S 0) T)
		; second condition is if the list is empty then return nil
		((and (null L) (not (= S 0))) '())
		;third condition is check if the sum can be made by either including or excluding the element
		(T (or (subset-sum S (cdr L)) (subset-sum (- S (car L)) (cdr L)))) 
	)
)

(defun subset-sum (S L)	
	(cond 
		; first condition is if the sum requested is zero then we can just return an empty list
		;((= S 0) '())
		((= S 0) T)
		; second condition is if the list is empty then return nil
		((and (null L) (not (= S 0))) '())
		;third condition is check if the sum can be made by either including or excluding the element
		((subset-sum (- S (car L)) (cdr L)) 
			(if (not (= (- S (car L))))
				((append (list (car L)) (subset-sum (- S (car L)) (cdr L))))
			)
		)
		;((subset-sum S (cdr L)) (append '() (subset-sum S (cdr L))))
	)
)


;QUESTION 6
