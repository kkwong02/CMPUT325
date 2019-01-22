; Cmput 325 Winter 2019 Assignment 1
; CCID kskwong Student name Kirsten Kwong

;QUESTION 1 issorted
; (len L)
; This function counts the number of items in a list, L and returns it.
; Taken (and renamed) from in-class examples.
(defun len (L)
    (if (null L) 
        0 
        (+  (len (cdr L)) 1) 
    )
)

; (issorted L)
; This function checks if a list, L, is sorted in ascending order.
; The function will return T if sorted, else it will return NIL
; A list with less than 2 elements is considered sorted.
(defun issorted (L)
    (if (OR (null L) (= 1 (len L)) )
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
        ((= (len L) 1) (car L))
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
        ((or (null L)  (= (len L) 1)) T)
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
; Unlike replace1, this function will recursive replace atoms in nested lists.
(defun replace2 (Atom1 Atom2 List)
    (cond
        ((null List) NIL)
        ((not (atom (car List))) (cons (replace2 Atom1 Atom2 (car List)) (replace2 Atom1 Atom2 (cdr List))))
        ((equal (car List) Atom1) (cons Atom2 (replace2 Atom1 Atom2 (cdr List))))
        (T (cons (car List) (replace2 Atom1 Atom2 (cdr List))))
    )
)

; QUESTION 5 common
; (in a L)
; A helper function that checks if an atom, A, is in list, L.
; returns T if a in L, else NIL
(defun in (a L)
    (cond 
        ((null L) NIL)
        ((equal a (car L)) T)
        (T (in a (cdr L)))
    )
)

; (common L1 L2)
; This function counts how many atoms L1 and L2 have in common.
(defun common (L1 L2)
    (cond
        ((null L1) 0)
        ((in (car L1) L2) (+ 1 (common (cdr L1) L2)))
        (T (common (cdr L1) L2))
    )
)

; QUESTION 6 Setcover

