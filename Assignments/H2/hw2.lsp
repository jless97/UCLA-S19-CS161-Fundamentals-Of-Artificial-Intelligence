;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; Function BFS takes a single argument FRINGE that represents a list of search
; trees, and returns a top-level list of leaf nodes, in the order they are 
; visited by left-to-right breadth-first search. The initial call to BFS 
; passes a FRINGE list with a single element: the root of the search tree.
(defun BFS (FRINGE)
  (cond 
    ; Empty, return empty list
    ((null FRINGE) nil)
    ; If the current node (i.e. front of list) is an atom, then process it and
    ; move to the next node in the current level
    ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
    ; Else if the current node is a list, then move on to the next node (i.e.
    ; back of list) and unpack the current list's contents to process next
    (t (BFS (append (cdr FRINGE) (car FRINGE))))
  )
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (cond
    ((equal S '(T T T T)) T)
    (t nil)
  )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes two arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (let ((homer (first S)) (baby (second S)) (dog (third S)) (poison (fourth S)))
    (cond 
      ; homer
      ((equal A 'h) 
        (cond 
          ; Invalid: moving homer results in baby being left alone with dog or poison
          ((and (equal homer baby) (or (equal baby dog) (equal baby poison))) nil)
          ; Else return next state
          (t (list (cons (not homer) (cdr S))))
        )
      )
      ; homer and baby
      ((equal A 'b) 
        (cond
          ; Impossible: homer and baby are currently on opposite sides
          ((not (equal homer baby)) nil)
          ; Else return next state
          (t (list (list (not homer) (not baby) dog poison)))
        )
      )
      ; homer and dog
      ((equal A 'd) 
        (cond
          ; Impossible: homer and dog are currently on opposite sides
          ((not (equal homer dog)) nil)
          ; Invalid: moving homer results in baby being left alone with poison
          ((equal baby poison) nil)
          ; Else return next state
          (t (list (list (not homer) baby (not dog) poison)))
        )
      )
      ; homer and poison
      (( equal A 'p) 
        (cond
          ; Impossible: homer and poison are currently on opposite sides
          ((not (equal homer poison)) nil)
          ; Invalid: moving homer results in baby being left alone with dog
          ((equal baby dog) nil)
          ; Else return next state
          (t (list (list (not homer) baby dog (not poison))))
        )
      )
      (t nil)
    )
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if S is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (let ((curr-state (car STATES)) (next-states (cdr STATES)))
    (cond
      ; Current state is not on the stack of states visited by this DFS
      ((null STATES) nil)
      ; Current state is on the stack of visited states
      ((equal S curr-state) T)
      ; Check remaining states in the stack of visited states
      (t (ON-PATH S next-states))
    )
  )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (let ((curr-state (car STATES)) (other-states (cdr STATES)))
    (cond
      ((null STATES) nil)
      ; Try to reach end goal from current state
      ; If no such path exists, then try the other states at the current level
      ((null (DFS curr-state PATH)) (MULT-DFS other-states PATH))
      ; Can reach end goal from current state
      (t (DFS curr-state PATH))
    )
  )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (let ((new-path (append PATH (list S))) (next-states (SUCC-FN S)))
    (cond
      ; DFS revisits a node already on the search path
      ((ON-PATH S PATH) nil)
      ; End goal reached
      ((FINAL-STATE S) new-path)
      ; Generate the next states that can be reached from the current state
      ; and DFS from those states to try to reach the end goal
      (t (MULT-DFS next-states new-path))
    )
  )
)
    
