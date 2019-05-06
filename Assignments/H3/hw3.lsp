;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond (
    (null s) nil)
    (t 
      (let ((x (getKeeperColumn (car s) 0)))
        (if x
		      ;keeper is in this row
		      (list x row)
		      ;otherwise move on
		      (getKeeperPosition (cdr s) (+ row 1))
		    );end if
      );end let
    );end t
	);end cond
);end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond (
    (null L) nil)
    (t 
      (let ((cur (car L)) (res (cleanUpList (cdr L))))
        (if cur 
		      (cons cur res)
		      res
		    );end if
      );end let
    );end t
	);end cond
);end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; goal-test takes a single argument s, the current state and returns
; true if it is a goal state (i.e. all boxes on star squares)
(defun goal-test (s)
  (cond
    ((null s) T)
    ((atom s)
      (cond 
        ; If there are any boxes, then this is not a goal state
        ((isBox s) nil)
        (t T)
      )
    )
    ; Recursively iterate through all squares to verify there are no boxes left
    (t (and (goal-test (car s)) (goal-test (cdr s))))
  )
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; get-elem is a helper function for get-square that takes two arguments row (the desired row of
; current state S) and c (column number), and returns the integer contents of this row at column c
(defun get-elem (row c)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ; State is wall
      ((null row) wall)
      ; Col c is out of scope
      ((< c 0) wall)
      ; We foiund the col we are looking for, return integer contents of square (r, c)
      ((= c 0) curr-elem)
      ; Recursively iterate through elems of this row until we find the one we want
      (t (get-elem other-elems (- c 1)))
    )
  )
)

; get-row is a helper function for get-square that takes two arguments S (the current state),
; and r (row number), and returns the row that get-square wants to get-square from
(defun get-row (s r)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond 
      ; State is null
      ((null s) nil)
      ; Row r is out of scope
      ((< r 0) nil)
      ; We found the row we are looking for
      ((= r 0) curr-row)
      ; Recursively iterate through rows until we find the one we want
      (t (get-row other-rows (- r 1)))
    )
  )
)

; get-square takes three arguments S (the current state), r (row number), and c (col number),
; and returns the integer content of state S at square (r, c). If the square is outside the
; scope of the problem, it returns the value of the wall
(defun get-square (s r c)
  (let ((row (get-row s r)))
    (get-elem row c)
  )
)

; set-row is a helper function for set-square and takes three arguments: row (the desired row
; of current state S), c (column number), and v (integer), and sets square with col c of this
; row with value v
(defun set-row (row c v)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ; State is null
      ((null row) nil)
      ; We found the col we are looking for, so set square (r, c) with value v
      ((= c 0) (cons v other-elems))
      ; Recursively iterate through elems of this row until we find the column we want
      (t (cons curr-elem (set-row other-elems (- c 1) v)))
    )
  )
)

; set-square takes four arguments S (the current state), r (row number), c (col number), and
; v (integer), and returns a new state S' that is obtained by setting square (r, c) to v. And
; does not modify the input state
(defun set-square (s r c v)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond 
      ; State is null
      ((null s) nil)
      ; We found the row we are looking for, set the square with content v in this row
      ((= r 0) (cons (set-row curr-row c v) other-rows))
      ; Recursively iterate through rows until we find the one we want
      (t (cons curr-row (set-square other-rows (- r 1) c v)))
    )
  )
)

; can-move is a helper function for try-move that takes two arguments: one-away (the integer contents
; of the square one step away from the keeper in move direction) and two-away (the integer contents
; of the square two steps away from the keeper in move direction), and returns true if the keeper can
; move in move direction
(defun can-move (one-away two-away)
  (cond
    ; Wall blocking path, return false
    ((isWall one-away) nil)
    ; Box one step away, but two steps away is a wall or another box, return false
    ((and (isBox one-away) (not (or (isStar two-away) (isBlank two-away)))) nil)
    (t T)
  )
)

; do-move is a helper function for try-move that takes eight arguments: s (the current state), 
; dir (move direction), k (integer contents of the keeper), r (row num), c (col num), op (operand
; specifying the direction we are moving), one-away (integer contents of square one step away from
; the keeper), and two-away (integer contents of square two steps away from the keeper), and updates
; the corresponding contents (at most 3 squares) of the current state by moving in move direction
(defun do-move (s dir k r c op one-away two-away)
  (cond
    ; Moving up or down
    ((equal dir 'VERTICAL)
      (cond
        ; curr = keeper, one-away = blank
        ((and (isKeeper k) (isBlank one-away)) 
          (set-square (set-square s r c blank) (funcall op r 1) c keeper)
        )
        ; curr = keeperstar, one-away = blank
        ((and (isKeeperStar k) (isBlank one-away))
          (set-square (set-square s r c star) (funcall op r 1) c keeper)
        )

        ; curr = keeper, one-away = star
        ((and (isKeeper k) (isStar one-away))
          (set-square (set-square s r c blank) (funcall op r 1) c keeperstar)
        )
        ; curr = keeperstar, one-away = star
        ((and (isKeeperStar k) (isStar one-away))
          (set-square (set-square s r c star) (funcall op r 1) c keeperstar)
        )

        ; curr = keeper, one-away = box, two-away = blank
        ((and (isKeeper k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeper) (funcall op r 2) c box)
        )
        ; curr = keeperstar, one-away = box, two-away = blank
        ((and (isKeeperStar k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeper) (funcall op r 2) c box)
        )

        ; curr = keeper, one-away = box, two-away = star
        ((and (isKeeper k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeper) (funcall op r 2) c boxstar)
        )
        ; curr = keeperstar, one-away = box, two-away = star
        ((and (isKeeperStar k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeper) (funcall op r 2) c boxstar)
        )

        ; curr = keeper, one-away = boxstar, two-away = blank
        ((and (isKeeper k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeperstar) (funcall op r 2) c box)
        )
        ; curr = keeperstar, one-away = boxstar, two-away = blank
        ((and (isKeeperStar k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeperstar) (funcall op r 2) c box)
        )

        ; curr = keeper, one-away = boxstar, two-away = star
        ((and (isKeeper k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeperstar) (funcall op r 2) c boxstar)
        )
        ; curr = keeperstar, one-away = boxstar, two-away = star
        ((and (isKeeperStar k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeperstar) (funcall op r 2) c boxstar)
        )
      )
    )
    ; Moving left or right
    ((equal dir 'HORIZONTAL)
      (cond
        ; curr = keeper, one-away = blank
        ((and (isKeeper k) (isBlank one-away)) 
          (set-square (set-square s r c blank) r (funcall op c 1) keeper)
        )
        ; curr = keeperstar, one-away = blank
        ((and (isKeeperStar k) (isBlank one-away))
          (set-square (set-square s r c star) r (funcall op c 1) keeper)
        )

        ; curr = keeper, one-away = star
        ((and (isKeeper k) (isStar one-away))
          (set-square (set-square s r c blank) r (funcall op c 1) keeperstar)
        )
        ; curr = keeperstar, one-away = star
        ((and (isKeeperStar k) (isStar one-away))
          (set-square (set-square s r c star) r (funcall op c 1) keeperstar)
        )

        ; curr = keeper, one-away = box, two-away = blank
        ((and (isKeeper k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeper) r (funcall op c 2) box)
        )
        ; curr = keeperstar, one-away = box, two-away = blank
        ((and (isKeeperStar k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeper) r (funcall op c 2) box)
        )

        ; curr = keeper, one-away = box, two-away = star
        ((and (isKeeper k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeper) r (funcall op c 2) boxstar)
        )
        ; curr = keeperstar, one-away = box, two-away = star
        ((and (isKeeperStar k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeper) r (funcall op c 2) boxstar)
        )

        ; curr = keeper, one-away = boxstar, two-away = blank
        ((and (isKeeper k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeperstar) r (funcall op c 2) box)
        )
        ; curr = keeperstar, one-away = boxstar, two-away = blank
        ((and (isKeeperStar k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeperstar) r (funcall op c 2) box)
        )

        ; curr = keeper, one-away = boxstar, two-away = star
        ((and (isKeeper k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeperstar) r (funcall op c 2) boxstar)
        )
        ; curr = keeperstar, one-away = boxstar, two-away = star
        ((and (isKeeperStar k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeperstar) r (funcall op c 2) boxstar)
        )
      )
    )
  )
)

; try-move takes two arguments: state S and move direction D, and returns the state that is the result
; of moving the keeper in state S in direction D. NIL is returned if the move is invalid (e.g. there
; is a wall in that direction)
(defun try-move (s dir)
  ; Gets the integer contents of the keeper (i.e. keeper or keeperstar)
  (let* (
    (pos (getKeeperPosition s 0))
    (c (car pos))
    (r (cadr pos))
    (k (get-square s r c)))

    (cond
      ; Move up => Update the integer contents of curr square, square one-step away, and square two-steps away
      ((equal dir 'UP)
        ; Get the integer contents of the squares one and two steps away
        (let* ((one-away (get-square s (- r 1) c)) (two-away (get-square s (- r 2) c)))
          (cond 
            ; If keeper can move in the move direction, then update the state
            ((equal (can-move one-away two-away) T)
              (do-move s 'VERTICAL k r c #'- one-away two-away)
            )
          )
        )
      )
      ; Move down => Update the integer contents of curr square, square one-step away, and square two-steps away
      ((equal dir 'DOWN)
        ; Get the integer contents of the squares one and two steps away
        (let* ((one-away (get-square s (+ r 1) c)) (two-away (get-square s (+ r 2) c)))
          (cond 
            ; If keeper can move in the move direction, then update the state
            ((equal (can-move one-away two-away) T)
              (do-move s 'VERTICAL k r c #'+ one-away two-away)
            )
          )
        )
      )
      ; Move left => Update the integer contents of curr square, square one-step away, and square two-steps away
      ((equal dir 'LEFT)
        ; Get the integer contents of the squares one and two steps away
        (let* ((one-away (get-square s r (- c 1))) (two-away (get-square s r (- c 2))))
          (cond 
            ; If keeper can move in the move direction, then update the state
            ((equal (can-move one-away two-away) T)
              (do-move s 'HORIZONTAL k r c #'- one-away two-away)
            )
          )
        )
      )
      ; Move right => Update the integer contents of curr square, square one-step away, and square two-steps away
      ((equal dir 'RIGHT)
        ; Get the integer contents of the squares one and two steps away
        (let* ((one-away (get-square s r (+ c 1))) (two-away (get-square s r (+ c 2))))
          (cond 
            ; If keeper can move in the move direction, then update the state
            ((equal (can-move one-away two-away) T)
              (do-move s 'HORIZONTAL k r c #'+ one-away two-away)
            )
          )
        )
      )
    )
  )
)

; next-states takes one argument: s (the current state), and returns a list of all possible states
; that are reachable from the current state
(defun next-states (s)
  (cleanUpList (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT)))
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;

; h0 is a trivially admissible heuristic function 
; h0 takes a single argument: s (the current state) and just returns the constant 0
(defun h0 (s)
  0
);end defun

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; h1 is an admissible heuristic function (hamming distance) that is admissible because
; any boxes not on stars need to move at least once (which is never more moves than the 
; actual cost)
; h1 takes a single argument: s (the current state) and returns the number of boxes not
; on goal squares
(defun h1 (s)
  (cond
    ((null s) 0)
    ((atom s)
      (cond
        ((isBox s) 1)
        (t 0)
      );end cond
    )
    (t (+ (h1 (car s)) (h1 (cdr s))))
  );end cond
); end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; set-coords is a helper function to h404640158 that takes two arguments: row-num and col-nums,
; and returns a list of coordinates of either boxes or stars for the current row
(defun set-coords (row-num col-nums)
  (let ((curr-col-num (car col-nums)) (other-col-nums (cdr col-nums)))
    (cond
      ((null col-nums) nil)
      (t (append (list (list row-num curr-col-num)) (set-coords row-num other-col-nums)))
    )
  )
)

; get-cols-in-row is a helper function to h404640158 that takes three arguments: row (the current row),
; object-type (whether we are looking for boxes or stars), and col-num, and returns the column numbers
; of all boxes or all stars in the current row
(defun get-cols-in-row (row object-type col-num)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ((null row) nil)
      (t 
        (cond
          ; We are looking for boxes
          ((equal object-type box)
            (cond
              ; Current elem is a box, so add it and recursively look for other boxes in this row
              ((isBox curr-elem) (cons col-num (get-cols-in-row other-elems object-type (+ col-num 1))))
              ; Current elem is not a box, recursively look for other boxes in this row
              (t (get-cols-in-row other-elems object-type (+ col-num 1)))
            )
          )
          ; We are looking for stars
          ((equal object-type star)
            (cond 
              ; Current elem is a star, so add it and recursively look for other stars in this row
              ((isStar curr-elem) (cons col-num (get-cols-in-row other-elems object-type (+ col-num 1))))
              ; Current elem is not a star, recursively look for other stars in this row
              (t (get-cols-in-row other-elems object-type (+ col-num 1)))
            )
          )
        )
      )
    )
  )
)

; get-coords is a helper function to h404640158 that takes three arguments: s (the current state), object-type
; (whether it is a box or star), and row-num, and returns the coordiantes of either all boxes or all stars
(defun get-coords (s object-type row-num)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond
      ((null s) nil)
      ; Get the coordinates of any boxes or stars for the current row
      (t (let* ((cols (get-cols-in-row curr-row object-type 0)) (coords (set-coords row-num cols)))
        (cond
          ; Recursively iterate through all rows to get coordinates of all boxes or all stars
          ((not (equal cols nil)) (append coords (get-coords other-rows object-type (+ row-num 1))))
          (t (get-coords other-rows object-type (+ row-num 1)))
        ))
      )
    )
  )
)

; dist-box-to-star is a helper function for h404640158 that takes two arguments: box-coord (the coordinates of a
; single box) and star-coord (the coordinates of a single star), and returns the distance from the box to the star
(defun dist-box-to-star (box-coord star-coord)
  (let ((box-row (first box-coord)) (box-col (second box-coord)) (star-row (first star-coord)) (star-col (second star-coord)))
    (cond 
      ((null box-coord) 0)
      ((null star-coord) 0)
      ((< box-row star-row)
        (cond 
          ((< box-col star-col) (+ (- star-row box-row) (- star-col box-col)))
          (t (+ (- star-row box-row) (- box-col star-col)))
        )
      )
      (t 
        (cond 
          ((< box-col star-col) (+ (- box-row star-row) (- star-col box-col)))
          (t (+ (- box-row star-row) (- box-col star-col)))
        )
      )
    )
  )
) 

; min-dist-box-to-star is a helper function for h404640158 that takes two arguments: box-coord (the coordinates
; of a single box) and star-coords (the coordinates of all stars), and returns the distance from the box
; to the closest star square
(defun min-dist-box-to-star (box-coord star-coords)
  (let ((star-coord (car star-coords)) (other-star-coords (cdr star-coords)))
    (cond 
      ((not box-coord) nil)
      ((not star-coords) nil)
      ; Get the current distance from the box to a single star, and distances from the box to the remaining stars
      (t (let ((curr-dist (dist-box-to-star box-coord star-coord)) (next-dist (min-dist-box-to-star box-coord other-star-coords)))
        (cond
          ((not next-dist) curr-dist)
          (t
            (cond
              ; Return the min distance from the box to a star
              ((< curr-dist next-dist) curr-dist)
              (t next-dist)
            )
          )
        ))
      )
    )
  )
)

; calculate-manhattan-distance is a helper function for h404640158 that takes two arguments: 
; box-coords (a list of coordinates of all boxes) and star-coords (a list of coordinates of all squares),
; and returns the manhattan distance of boxes to get to star squares
(defun calculate-manhattan-distance (box-coords star-coords)
  (let ((box-coord (car box-coords)) (other-box-coords (cdr box-coords)))
    (cond
      ((not box-coord) 0)
      ((not star-coords) 0)
      ; Recursively calculates the min distance for each box to get to a star square, and adds them up
      (t (+ (min-dist-box-to-star box-coord star-coords) (calculate-manhattan-distance other-box-coords star-coords)))
    )
  )
)

; h404640158 is an admissible heuristic function that calculates the manhattan distance from each
; box to get to a goal state
; h404640158 takes a single argument: s (the current state) and returns the manhattan distance for
; boxes to get to star squares
(defun h404640158 (s)
  ; Get the coordinates of all boxes and all stars
  (let ((box-coords (get-coords s box 0)) (star-coords (get-coords s star 0)))
    ; Calculate the manhattan distance from each box to get to a star square
    (calculate-manhattan-distance box-coords star-coords)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* (
    (k1 (getKeeperPosition s1 0))
	  (k2 (getKeeperPosition s2 0))
	  (deltaX (- (car k2) (car k1)))
	  (deltaY (- (cadr k2) (cadr k1)))
	)
    (cond 
      ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	    (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
  );end let
);end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond 
    ((null m) nil)
	  ((= 1 (length m)) (list 'END))
	  (t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
);

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond 
    ((= s blank) (format t " "))
	  ((= s wall) (format t "#"))
	  ((= s box) (format t "$"))
	  ((= s keeper) (format t "@"))
	  ((= s star) (format t "."))
	  ((= s boxstar) (format t "*"))
	  ((= s keeperstar) (format t "+"))
	  (t (format t "|"))
	);end cond
)

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
    )
  );end progn
)

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
  );end dolist
);end defun
