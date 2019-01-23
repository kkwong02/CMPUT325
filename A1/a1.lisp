; Cmput 325 Winter 2019 Assignment 1
; CCID kskwong Student name Kirsten Kwong

;QUESTION 1 issorted
; (issorted L)
; This function checks if a list, L, is sorted in ascending order.
; The function will return T if sorted, else it will return NIL
; A list with less than 2 elements is considered sorted.
(defun issorted (L)
    (if (OR (null L) (null cdr(L)) )
        T
        (if (< (car L) (cadr L))
            (and T (issorted (rest L)))
            NIL
        )
    )
)

; QUESTION 2 numbers
; (numbers N)
; this function produces a list of integers from 1 up to N, given a non negative
; integer. the function will return NIL if the number given is less than 1.
(defun numbers (N)
    (if (< N 1)
        NIL
        
        (if (= N 1)
            (list 1) 
            (cons (numbers (- N 1)) N)
        )
    )
)

; QUESTION 3 palindrome
; (len L)
; This function counts the number of items in a list, L and returns it.
; Taken (and renamed) from in-class examples.
(defun len (L)
    (if (null L) 
        0 
        (+  (len (cdr L)) 1) 
    )
)

; (splice L)
; A helper function for splicing a list, L.
; Removes the first and last elements of a list. Will return NIL if less than 3 elements.
(defun splice (L)
    (cond 
        ((< (len L) 3) NIL)
        ((= (len L) 3) (list (cadr L)))
        (T (cons (cadr L) (splice (cdr L))))
    )
)

; (getlast L)
; A helper function that gets the last element of a list
(defun getlast (L)
    (cond 
        ((null (cdr L)) (car L))
        ((null L) NIL)
        (T (getlast (cdr L))) 
    )
)

; (palindrome L )
; This function checks if a list of characters is a palindrome by recursively checking the 
; first and last characters.
(defun palindrome (L)
    (cond 
        ; length = 0 or length = 1 ==> T
        ((or (null L)  (null (cdr L))) T)
        ((eq (car L) (getlast L)) (and T (palindrome (splice L))))
        (T NIL)
    )
)

; QUESTION 4 replace1 and replace2
; (replace Atom1 Atom2 List)
; This function replace occurances of Atom1 with Atom2 in a list
; It does not recursively replace atoms in nested lists.
(defun replace1 (Atom1 Atom2 List)
    (cond
        ((null List) NIL)
        ((equal (car List) Atom1) (cons Atom2 (replace1 Atom1 Atom2 (cdr List))))
        (T (cons (car List) (replace1 Atom1 Atom2 (cdr List))))
    )
)

; (replace Atom1 Atom2 List)
; This function replaces occurances of Atom1 with Atom2 in a list.
; Unlike replace1, this function will recursively replace atoms in nested lists.
; The implementation of replace2 is identical to replace1, with the exception on
; on extra conditional check for non-atom list items, where there is an additional call
; to replace2 instead of either leaving or replacing the first element it the list.
(defun replace2 (Atom1 Atom2 List)
    (cond
        ((null List) NIL)
        ((not (atom (car List))) (cons (replace2 Atom1 Atom2 (car List)) (replace2 Atom1 Atom2 (cdr List))))
        ((equal (car List) Atom1) (cons Atom2 (replace2 Atom1 Atom2 (cdr List))))
        (T (cons (car List) (replace2 Atom1 Atom2 (cdr List))))
    )
)

; QUESTION 5 common
; (common L1 L2)
; This function counts how many atoms L1 and L2 have in common.
; This function works by recusively checking if the first element in L1
; is in L2, if yes, it adds one to the result else, it adds zero. It then
; continues with a recursive call using the rest of L1. 
(defun common (L1 L2)
    (cond
        ((null L1) 0)
        ((member (car L1) L2) (+ 1 (common (cdr L1) L2)))
        (T (common (cdr L1) L2))
    )
)

; QUESTION 6 Setcover

; (findFirst N S)
; A helper function that finds the largest subset.
; if all are equal, it returns the first (left-most) subset.
; N is the length of the current largest subset.
; S is a list of subsets.
; current is the current largest subset.
(defun findFirst (current N S) 
    (if (null S)
        current
        (let* 
            ((firstItem (car S))
            (lenFirst (len firstItem)))
            (if (< N lenFirst) 
                (findFirst firstItem lenFirst (cdr S))
                (findFirst current N (cdr S))
            )
        )  
    ) 
)
;; if equal, keep current (would be leftmost item)
;; else replace with (cons (car S) (len (car s)))
)

; (findLeastCommon L1 L2)
; This helper function finds the set with the least common 
; elements of L. 
; current: the curent set with the least common elements
; currentCount: the count of common elements in between current and L
; L a set of numbers we're finding the set of least common numbers for
; S list of sets
(defun findLeastCommon (current currentCount L S)
    (if (null S)
        current
        (let* 
            ((firstItem (car S))
            (firstItemCount (common L firstItem)))
            (if (> currentCount firstItemCount) 
                (findLeastCommon firstItem firstItemCount L (cdr S))
                (findLeastCommon current currentCount L (cdr S))
            )
        )
    ) 
)

; (insert L item)
; inserts an item into a sorted list.
; TODO: add a condition for last item in L.
(defun insert (L item) 
    (cond 
        ((null (car L)) (cons item L))
        ((> item (car L)) (cons (car L) (insert (cdr L) item)))
        (T (cons item L))
    )
)

; (combineSets (L1 L2))
; This helper functions combines the two lists in a ascending order.
; It assumes that both lists are sorted to begin with.
(defun combineSets (L1 L2)
    (if (null L2) 
        L1
        (combineSets (insert L1 (car L2)) (cdr L2))
    )
)


; (findRest N S)
; A helper function finds subsets that fit in setcover.
; This function works by finding the subset with the 
; least amount of items in common with the
; current: the current set
; subsets: the subsets that make up the set
; N: from setCover
; S: from setCover
(defun findRest (current subsets N S)
    (if (equal current (numbers S)) 
        subsets
        
        (let (LeastCommonSubset (findLeastCommon NIL (+ N 1) current S)))
    )
)

; (setCover N S)
; This function finds the set cover of a set of numbers from 1 to N
; given a list of subsets, S
(defun setcover (N S)
    ; combine the largest subset with everything else.
    (let (firstItem (findFirst (car S) (len (car s)) S) 
        (findRest firstItem (cons firstItem NIL) N S)
    )
)