; Cmput 325 Winter 2019 Assignment 1
; CCID kskwong Student name Kirsten Kwong

;QUESTION 1 issorted
; (len(L))
; This function counts the number of items in a list, L and returns it.
; Taken (and renamed) from in-class examples.
(defun len (L)
    (if (null L) 
        0 
        (+  (len (rest L)) 1) 
    )
)

; (issorted(L))
; This function checks if a list, L, is sorted in ascending order.
; The function will return T if sorted, else it will return NIL
; A list with less than 2 elements is considered sorted.
(defun issorted (L)
    (if (OR (= 0 (len L)) (= 1 (len L)) )
        T
        (if (< (first L) (first(rest L)))
            (issorted (rest L))
            NIL
        )
    )
)


;QUESTION 2 numbers
; (numbers N)
; this function produces a list of integers from 1 up to N, given a non negative integer.
(defun numbers (N)
    (if (< N 1)
        NIL
        
        (if (= N 1)
            (list 1)
            (append (numbers (- N 1)) (list N))
        )
    )
)

;QUESTION 3 palindrome
