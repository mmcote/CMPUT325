;QUESTION 1\
; Create the lisp function:
; - returns true if argument Y is a member of the argument list X
; - This should also test for lists being members of lists
; - The argument Y may be nil or a list containing nil

(defun xmember (X Y)
	; check if the list is empty
	(cond
		; if the list is empty it clearly is not in the list
		((null X) NIL)
		; check if the first element is equal to the requested item
		((equal (car X) Y) T)
		; else call function again to compare next element
		(T (xmember (cdr X) Y))
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
(defun split (L)
	(cond 
		((null L) (NIL NIL))
		((null (cddr L)) (append (list (list (car L))) (list (cdr L))))
		(T (let ((tuple (split (cddr L))))
				(append (list (append (list (car L)) (car tuple))) (list (append (list (cadr L)) (cadr tuple))))))
	)
)

;Question 6
; Once an element is found in the original list that is part of the solution we can remove it from the solution to prevent duplicates, a longer list, and more importantly an incorrect solution to subsetsum
(defun removeMatchedElement (sortedList originalElement)
	(cond
		; sortedList is empty and the matched element was not found return nil
		((null sortedList) NIL)
		; skip the matched element and return the rest of the sortedList
		((= (car sortedList) originalElement) (cdr sortedList))
		; not the matched element append the current element and explore the rest of the sortedList
		(T (append (list (car sortedList)) (removeMatchedElement (cdr sortedList) originalElement)))
	)
)

; Assumption all of the elements in the sortedList are elements of the originalList
(defun restoreOrder (sortedList originalList)
	(cond
		; First Base Case: The orignal list is now empty 
		((null sortedList) NIL)
		; If the original element is part of the solution then append the original element, and remove the original element from the solution
		((xmember sortedList (car originalList))
			(let ((originalElement (car originalList)))
				(append (list originalElement) (restoreOrder (removeMatchedElement sortedList originalElement) (cdr originalList) ) )))
		; Else skip the original list element since it is not in the solution
		(T (restoreOrder sortedList (cdr originalList)))
	)
)

; Only optimization performed is passing a sorted list, hence the new base case
(defun optimizedSubsetSum (S L)	
	(cond 
		; First base case: The list is empty, therefore no elements to elimate remainder of sum.
		((null L) NIL)
		; Optimization base case: The current element being tested is greater than the sum. Therefore as this is sorted all those after must be greater as well.
		((> (car L) S) NIL)
		; Second base case: The current element being tested equals the current sum. Subset Found.
		((= (car L) S) (list (car L)))
		(T (let ((include (optimizedSubsetSum (- S (car L)) (cdr L)))
					(exclude (optimizedSubsetSum S (cdr L))))
				(cond
					; if a solution was found with the element append the element
					((not (null include)) (append (list (car L)) include))
					; if a solution was found without the element just explore the rest of the list
					((not (null exclude)) exclude)
					(T NIL)
				)
			)
		)
	)
)

; function subsetsum will attempt to find a subset of numbers in a given list that will sum up to the number requested
(defun subsetsum (S L)
	(restoreOrder (optimizedSubsetSum S (sort (copy-list L) #'<)) L)
)