;;; Question 1

;; Function PAD takes a single arg N and returns the Nth Padovan number
; Explanation: ecursively generates the Nth Padovan number
(defun PAD (N)
  (if (or (= N 0) (= N 1) (= N 2))
    1
    (+ (PAD (- N 2)) (PAD (- N 3)))
  )
)


;;; Question 2

;; Function SUMS takes a single arg N and returns the number of additions 
;; required by the PAD function to compute the Nth Padovan number
; Explanation: Recursively generates the number of additions similar to P1
(defun SUMS (N)
  (if (or (= N 0) (= N 1) (= N 2))
    0
    (+ 1 (SUMS (- N 2)) (SUMS (- N 3)))
  )
)


;;; Question 3

;; Function ANON takes a single argument TREE that represents a tree, and 
;; returns an anonymized tree with the same structure, but where all symbols 
;; and numbers are replaced by a question mark
; Explanation: if the tree is
;   (1) empty, return empty list
;   (2) single node, replace it with a question mark
;   (3) else, recursively convert the list by calling ANON on car and cdr 
(defun ANON (TREE)
  (cond ((not TREE) nil)
        ((atom TREE) '?)
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
  )
)