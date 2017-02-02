;QUESTION 1\
; Create the lisp function:
; - returns true if argument Y is a member of the argument list X
; - This should also test for lists being members of lists
; - The argument Y may be nil or a list containing nil
;
; Check if the list X is NIL, return NIL. 
; Else check if first element of X is equal to Y, return T.
; Else recurse by checking the next element in the list X by calling
; xmember with the rest of the list
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
;
; ASSUMPTIONS
; - ONLY ATOMS OR LISTS
;
; Check if the list X is null, return NIL
; Else check if first element in X is an atom, append to list and recurse flatten
; on the rest of the list X
; Else call flatten on the first element of the list X since it must be an element
; and on the rest of the list X
(defun flatten (X)
	; check if the list is now empty
	(cond ((null X) NIL)
		; take the first item and check if atom, append to return list if atom
		((atom (car X)) (append (list (car X)) (flatten (cdr X))))
		; if not atom we assume it is a list and should explore
		(T (append (flatten (car X)) (flatten (cdr X))))
	)
)


;QUESTION 3
; Create a lisp function:
; - mixes the elements of two lists L1 and L2
; - returning a single list

; This function will handle one list at a time and alternate them on each recursive call
; Check if L2 is null, return L1 to be appended for the end of the list
; Else append the first element L2 and recurse by calling mix but alternating the lists
(defun mix (L1 L2)
	; check if either of the list are empty
	(cond ((null L2) L1)
		; add one element of L1 to the mixed list
		(T (append (list (car L2)) (mix (cdr L2) L1)))
	)
)

;QUESTION 4
; Create a lisp function:
; - Splits the elements of one list into two new list L1 and L2, held within one list
;
; Check if L is null, else return a list of two NILs, (NIL NIL)
; Else check if the list after taking off the first two is null, then return the two last elements as
; a list of lists
; Else append the two first elements to the each list, L1 and L2, then recurse and call split on the rest of the list
(defun split (L)
	(cond 
		((null L) (cons L (cons L L)))
		((null (cddr L)) (append (list (list (car L))) (list (cdr L))))
		(T (let ((tuple (split (cddr L))))
				(append (list (append (list (car L)) (car tuple))) (list (append (list (cadr L)) (cadr tuple))))))
	)
)

#|
#5.1
Let L1 and L2 be lists. Is it always true that (split (mix L2 L1)) returns the list (L1 L2)? If yes, give a proof. 
If no, describe exactly for which pairs of lists L1, L2 the result is different from (L1 L2).
- This is only true in certain cases such as the lists are of the same length, L1 is at most one less than L2, etc..
But overall this is not true. For instance if L2 is even one greater than L1:
(split (mix '(1 2) '(3))) -> ((3 2) (1))
Or if one of the lists is greater in length by more than one:
(split (mix '(1 2 3) '(4))) -> ((4 2) (1 3))
Therefore this is not gurenteed to return the same lists in opposite ordering.

#5.2
True. It is always true that (mix (cadr (split L)) (car (split L))) returns L. Number the elements of L by there nth
position, (L0, L1, ... LN). 

Return all of the odd elements
(cadr (split L)) = (cadr ((L0 L2 ... LN) (L1 L3 ... LN-1)) ) = (L1 L3 ... LN-1)
Return all of the even elements
(car (split L)) = (car ((L0 L2 ... LN) (L1 L3 ... LN-1)) ) = (L0 L2 ... LN)
Return the list of combining the two lists starting with the list on the left.
(mix (L0 L2 ... LN) (L1 L3 ... LN-1)) = (L0 L1 L2 L3 ... LN)

As there is zero chance of having a list with a difference in length of greater than one 
from the split, one can tell from the two functions that we are guarenteed to get the 
same list back.
|#

;Question 6
; function subsetsum will attempt to find a subset of numbers in a given list that will sum up to the 
; number requested this is done by calling the optimizedsum, then reordering the list since the original
; list order is lost when sorting the list originally
(defun subsetsum (S L)
	(restoreOrder (optimizedSubsetSum S (sort (copy-list L) #'<)) L)
)

; function optimization sum works by taking in a sorted list L
; check if list L is empty, return NIL
; check if the first element is greater than the current sum, (Impossible to get sum, since this works by decrementing), return NIL
; check if the first element is equal to the current sum, then return the element
; else call optimized subsetsum including the first element
;	check if a solution was reached with the first element, if so append the result
; 	else call optimized subsetsum without the first element and see if a solution was reached, if so append the result
;	else return NIL
; Only optimization performed is passing a sorted list, hence the new base case
(defun optimizedSubsetSum (S L)	
	(cond 
		; First base case: The list is empty, therefore no elements to elimate remainder of sum.
		((null L) NIL)
		; Optimization base case: The current element being tested is greater than the sum. Therefore as this is sorted all those after must be greater as well.
		((> (car L) S) NIL)
		; Second base case: The current element being tested equals the current sum. Subset Found.
		((= (car L) S) (list (car L)))
		(T (let ((include (optimizedSubsetSum (- S (car L)) (cdr L))))
				(cond
					; if a solution was found with the element append the element
					((not (null include)) (append (list (car L)) include))
					; if a solution was found without the element just explore the rest of the list
					(T (let ((exclude (optimizedSubsetSum S (cdr L))))
						(cond
							((not (null exclude)) exclude)
							(T NIL)
						)))
					
				)
			)
		)
	)
)

; when restoring order this function is used to remove the element in the solution to prevent duplicates
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