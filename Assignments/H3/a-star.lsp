#|
This is the code for an implementation of A* and its supporting functions.
Do not modify the content of this file.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The priority queue for the open list
;;
;; This priority queue will work for problems where the the cost is equal
;; to the depth.  It is an array where element 4999 indicates the number of
;; elements and element 5000 indicates the cost of the least expensive
;; element Slot i < 4999 is a list of nodes with total cost i.  Using this
;; type of pq is more efficient than a heap for problems where cost = depth.

(defun pq-create ()
  (let ((ans (make-array 5001 :initial-element nil)))
    (setf (aref ans 4999) 0)
    (setf (aref ans 5000) 100000)
    ans))

(defun pq-add (pq my_path new-cost)
  (let ((old-size (aref pq 4999))
        (old-cost (aref pq 5000)))
    (setf (aref pq 4999) (+ 1 old-size))
    (setf (aref pq 5000) (min old-cost new-cost))
    (setf (aref pq new-cost) (cons my_path (aref pq new-cost)))))

(defun pq-lowest-cost (pq start)
  (let ((old-size (aref pq 4999)))
    (cond ((= old-size 0) 100000)
          (t (let ((i start))
            (loop (when (aref pq i) (return i)) (setf i (+ i 1))))))))

(defun pq-remove (pq)
  (let ((old-size (aref pq 4999))
        (old-cost (aref pq 5000)))
    (if (= old-size 0) nil
      (let* ((ans (pop (aref pq old-cost)))
             (new-size (- old-size 1)))
        (setf (aref pq 4999) new-size)
        (setf (aref pq 5000) (pq-lowest-cost pq old-cost))
        ans))))

(defun pq-peek (pq)
  (let ((old-cost (aref pq 5000))
        (old-size (aref pq 4999)))
    (if (= old-size 0) nil
      (first (aref pq old-cost)))))

(defun pq-empty (pq)
  (let ((old-size (aref pq 4999)))
    (= old-size 0)))

;;;;;;;;;;;;;;;;;;;;;;;
;; The hash table for the closed list
;;
;; Lisp hash tables were found to be horrible.  As a result, I defined my
;; own.  This is not a general hash table.  But it will work in conjunction
;; with a*.  It will not grow, so the initial size should be suffiently
;; large.  The ht is a list, where the first element is the size, and the
;; second element is an array of collision lists.  An element in a collision
;; list is a key/value pair.


(defun hash-helper (start l)
  (cond ((null l) start)
        (t (hash-helper (+ (car l) (* start 65559)) (cdr l)))))

(defun hash-fn (s)
  (let ((ans 0))
    (setf ans (hash-helper ans (first s)))
    (setf ans (hash-helper ans (cons 0 nil)))
    (setf ans (hash-helper ans (second s)))
    (setf ans (hash-helper ans (cons 0 nil)))
    (setf ans (hash-helper ans (third s)))
    (setf ans (hash-helper ans (cons 0 nil)))
    (setf ans (hash-helper ans (fourth s)))))

;; Recursive part of myht-get.  Finds the given key and returns its value.

(defvar rcalls 0)
(defvar getcalls 0)

(defun myht-get-r (key collision)
  (incf rcalls)
  (cond ((null collision) nil)
        ((equal (first (first collision)) key) (second (first collision)))
        (t (myht-get-r key (cdr collision)))))

;; Recursive part of myht-remove.  Finds the given key and removes it.
;; Assumes that the key exists.

(defun myht-remove-r (key collision)
  (cond ((equal (first (first collision)) key) (cdr collision))
        (t (cons (car collision) (myht-remove-r key (cdr collision))))))

;; Crreates a ht.

(defun myht-create (size)
  (list size (make-array size :initial-element nil)))

;; Removes the given key, which is assumed to exist.

(defun myht-remove (ht key hash-val)
  (let* ((size (first ht))
         (index (mod hash-val size))
         (collisions (second ht)))
    (setf (aref collisions index)
          (myht-remove-r key (aref collisions index)))))

;; Adds the given key/value pair, assuming the key does not already exist.

(defun myht-add (ht key value hash-val)
  (let* ((size (first ht))
         (index (mod hash-val size))
         (collisions (second ht)))
    (setf (aref collisions index)
          (cons (list key value) (aref collisions index)))))

;; Returns the value corresponding to the given key or nil if key does not
;; exist.
(defun myht-get (ht key hash-val)
  (incf getcalls)
  (let* ((size (first ht))
         (index (mod hash-val size))
         (collisions (second ht)))
    (myht-get-r key (aref collisions index))))
 
;;;;;;;;;;;;;;;;;;;;;;;
;; The A* implementation

(defstruct (my_path)
  "my_path consists of a state, its parent, the cost from the root to the state,
and the total cost of the state (f(n) = g(n) + h(n))"
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun my_path-states (my_path)
  "collect all the states along this my_path."
  (unless (null my_path)
    (cons (my_path-state my_path)
          (my_path-states (my_path-previous my_path)))))

(defvar expanded 0)
(defvar generated 1)
 
(defun a* (start-state goal-p successor-fn heuristic-fn)
  (setf expanded 0)
  (setf generated 0)
  (setf rcalls 0)
  (setf getcalls 0)
  (let ((open-list (pq-create))
	(closed-list (myht-create 1000000)))
    (pq-add open-list (make-my_path :state start-state) 0)
    (let ((my_path (nreverse
		    (my_path-states
		     (a*-search open-list
				goal-p
				successor-fn
				#'(lambda (x y) (declare (ignore x y)) 1)
				heuristic-fn
				closed-list)))))
      (format t "My_Path: ~A~%" my_path)
      (format t "Nodes Generated by A*: ~A~%" generated)
      (format t "Nodes Expanded by A*: ~A~%" expanded)
      (format t "Solution depth: ~A~%" (- (length my_path) 1))
      (format t "rcalls: ~A~%" rcalls)
      (format t "getcalls: ~A~%" getcalls)
      my_path)))
    
;; find the my_path from start state to a state that satisfies goal-p.
;; Evaluate each state using the sum of cost-fn and remaining-cost-fn.
;; Return the my_path"

(defun a*-search (open-list goal-p successors cost-fn remaining-cost-fn closed-list)
  (loop
     (when (pq-empty open-list) (return nil))
     (when (funcall goal-p (my_path-state (pq-peek open-list)))
       (return (pq-peek open-list)))
     (let* ((my_path (pq-remove open-list))
	    (state (my_path-state my_path))
	    (new-val (my_path-total-cost my_path))
	    (hash-val (hash-fn state))
	    (closed-val (myht-get closed-list state hash-val)))
       (when (or (not closed-val) (< new-val closed-val))
	 (incf expanded)
	 (if closed-val (myht-remove closed-list state hash-val))
	 (myht-add closed-list state new-val hash-val)
	 (dolist (state2 (funcall successors state))
	   (let* ((cost (+ (my_path-cost-so-far my_path)
			   (funcall cost-fn state state2)))
		  (cost2 (funcall remaining-cost-fn state2))
		  (total-cost (+ cost cost2)))
	     (incf generated)
	     (pq-add open-list
		     (make-my_path
		      :state state2 
		      :previous my_path
		      :cost-so-far cost
		      :total-cost total-cost) 
		     total-cost)))))))
