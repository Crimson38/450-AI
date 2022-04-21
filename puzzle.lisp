; Puzzle.lisp 
; Author: Drew Walizer
; Date:

; Checks if the state is the goal state.
; Returns T is true Nil if false.
(defun goal-state (lst)
(equal lst '(1 2 3 8 e 4 7 6 5)))

(defun get-goal-state ()
'(1 2 3 8 e 4 7 6 5)
)

; Returns the direction of the move U, D, L, R.
(defun get-direction (lst)
(car lst))

; Returns the state of the list
(defun get-state (lst)
(cadr lst))

; Checks if the two states are equal.
; Returns T if true and Nil if false.
(defun same-state (lst lst2)
(equal (get-state lst) (get-state lst2)))

; Returns a list of directions went. 
(defun path (lst)
(cond
((null (cadr lst)) (caar lst))
(T (append (path (cdr lst)) (list (caar lst))))))

; Remove redundant states that appear in lst and lst2. 
; Return lst without redundant states.  
(defun remove-redundant (lst lst2)
(cond
((null lst) lst)
((null lst2) lst)
(T (remove-nil (append (list (remove-redundant-helper (car lst) lst2)) (remove-redundant (cdr lst) lst2))))
))

; Takes a move from list 1 and recurses through list 2.
; If the state is in list 2 return nil else return the state.
(defun remove-redundant-helper (lst lst2)
(cond
((null lst2) lst)
((null (same-state lst (car lst2))) (remove-redundant-helper lst (cdr lst2)))
(T nil)
))

; Removes all nils from the list.
; Returns new list of moves.
(defun remove-nil (lst)
(cond
((null lst) nil)
((null (car lst)) (remove-nil (cdr lst)))
(T (append (list (car lst)) (remove-nil (cdr lst))))
)) 

; Takes a list or state of the puzzle.
; Returns a list of all the possible moves and their resulting states.
(defun moves (lst)
(append
(check-move-up lst (position 'e lst))
(check-move-down lst (position 'e lst))
(check-move-left lst (position 'e lst))
(check-move-right lst (position 'e lst))
))

; Checks to see if the empty space can move up by checking pos or the postion of e.
; If true swap empty space with space above.
(defun check-move-up (lst pos)
(cond
((< pos 3) nil)
(T (list (append (list 'U) (list (swap lst (nth (- pos 3) lst))))))
))

; Checks to see if the empty space can move down by checking pos or the postion of e.
; If true swap empty space with space below.
(defun check-move-down (lst pos)
(cond
((null lst) nil)
((> pos 5) nil)
(T (list (append (list 'D) (list (swap lst (nth (+ pos 3) lst))))))
))

; Checks to see if the empty space can move left by checking pos or the postion of e.
; If true swap empty space with space to the left.
(defun check-move-left (lst pos)
(cond
((equal (mod pos 3) 0) nil)
(T (list (append (list 'L) (list (swap lst (nth (- pos 1) lst))))))
))

; Checks to see if the empty space can move right by checking pos or the postion of e.
; If true swap empty space with space to the right.
(defun check-move-right (lst pos)
(cond
((equal (mod pos 3) 2) nil)
(T (list (append (list 'R) (list (swap lst (nth (+ pos 1) lst))))))
))

; Swaps postions with e and num.
; Returns the new list. 
(defun swap (lst num)
(cond 
((null lst) nil)
((equal (car lst) 'e) (append (list num) (swap (cdr lst) num)))
((equal (car lst) num) (append (list 'e) (swap (cdr lst) num)))
(T (append (list (car lst)) (swap (cdr lst) num)))
))

; Takes in the initial state. 
; Returns the initial path. 
; ex: (make-open-init ‘(2 8 3 1 6 4 7 e 5)) -> returns (((NIL (2 8 3 1 6 4 7 e 5))))
(defun make-open-init (lst)
(list (list (append (list nil) (list lst))))
)

; Takes a path of states.
; Returns a list of all possible extensions of the path. 
(defun extend-path (path)
(build-paths (remove-redundant (moves (get-state (car path))) path) path) 
)

; Takes all possible new moves and current path.
; Creates a new path for each possible move.
; Returns a list of all possible paths. 
(defun build-paths (newMoves path)
(cond
((null newMoves) nil)
(T (append (list (append (list (car newMoves)) path)) (build-paths (cdr newMoves) path)))
))

; Takes a list of paths.
; Uses breath-first-search to search the paths for a solution to the 8-puzzle.
; If a solution is found a list of moves to reach the solution is returned. 
; If a solution is not found or the root path is the solution returns nil.
(defun search-bfs (pathLst)
(cond 
((null pathLst) nil)
((goal-state (get-state (car (car pathLst)))) (path (car pathLst)))
(T (search-bfs(append (cdr pathLst) (extend-path (car pathLst)))))
))

; Takes a list of paths and a depth.
; Uses depth-first-search with a depth limit to search the paths for a 
;      solution to the 8-puzzle.
; If a solution is found a list of moves to reach the solution is returned. 
; If a solution is not found or the root path is the solution returns nil.
(defun search-dfs-fd (pathLst d)
(cond
((null pathLst) nil)
((goal-state (get-state (car (car pathLst)))) (path (car pathLst)))
((equal d (- (list-length (car pathLst)) 1)) (search-dfs-fd (cdr pathLst) d))
(T (search-dfs-fd (append (extend-path (car pathLst)) (cdr pathLst)) d))
))

; Takes a list of paths.
; Uses iterative deepening to search the paths for a solution to the 8-puzzle.
; If a solution is found a list of moves to reach the solution is returned. 
(defun search-id (pathLst)
(cond
((null pathLst) nil)
(T (search-id-helper pathLst (list-length (car pathLst))))
))

; Takes a list of paths and depth.
; After every depth-first-search increase the depth limit.
; If a solution is found a list of moves to reach the solution is returned. 
(defun search-id-helper (pathLst d)
(cond
((null pathLst) nil)
((null (search-dfs-fd pathLst d)) (search-id-helper pathLst (+ d 1)))
(T (search-dfs-fd pathLst d))
))

; Takes a state of the board. 
; Returns the number of tiles that are out of place.
; Does not count e as out of place.
(defun out-of-place (state)
(helper-out-of-place state (get-goal-state) 0)
)

; Takes a state of the board, the goal state, and a number num
; Num is the number of tiles out of place. 
; After comparing state to goal return num. 
(defun helper-out-of-place (state goal num)
(cond 
((null state) num)
((or (equal (car state) 'e) (equal (car state) (car goal))) (helper-out-of-place (cdr state) (cdr goal) num))
(T (helper-out-of-place (cdr state) (cdr goal) (+ num 1)))
))

; Finds the f-value of the path. 
; f-value is the g-cost + the h-cost.
; g-cost is the depth of the path (inital state = depth 0).
; h-cost is the number of nodes out of place. 
(defun out-of-place-f (path)
(+ (- (list-length path) 1) (out-of-place (get-state (car path)))) 
)

; Finds the h-cost of a state using manhattan distance.
; Takes a state of the board.
; Returns the h-cost of the state.
(defun manhattan (state)
(helper-manhattan state (get-goal-state) 0)
)

; Takes a state of the board, the goal state of the board, and an h-cost. 
; h-cost is the current manhattan distance for the state.
; finds the postition every tile in the current and goal state. 
; returns the total h-cost.
(defun helper-manhattan (state goal hcost)
(cond 
((null state) hcost)
((equal (car state) 'e) (helper-manhattan (cdr state) goal hcost))
(T (helper-manhattan (cdr state) goal (helper-manhattan-x (- 9 (list-length state)) (position (car state) goal) hcost)))
))

; Takes cpos: the current position of a tile in the current state.
;       gpos: the goal position of the same tile as cpos.
;       h-cost: the current manhattan distance for the state.
; Finds the displacement in the x for cpos and gpos.        
(defun helper-manhattan-x (cpos gpos hcost)
(helper-manhattan-y cpos gpos (- (mod gpos 3) (mod cpos 3)) hcost)
)

; Takes cpos: the current position of a tile in the current state.
;       gpos: the goal position of the same tile as cpos.
;       x: displacement in the x for cpos and gpos.
;       h-cost: the current manhattan distance for the state.
; Finds the the displacement in the y for cpos and gpos.
; Adds x and the displacement in the y to h-cost. 
(defun helper-manhattan-y (cpos gpos x hcost)
(let    ((shifted_cpos (+ cpos x)) 
        (new_hcost (+ (abs x) hcost))
        )
    (cond
    ((equal shifted_cpos gpos) new_hcost)
    ((or (equal (+ shifted_cpos 3) gpos) (equal (- shifted_cpos 3) gpos)) (+ new_hcost 1))
    (T (+ new_hcost 2))
    )
))

; Finds the f-value of the path.
; f-value is the g-cost + h-cost.
; g-cost is the depth of the path (inital state = depth 0).
; h-cost is the manhattan distace of the state. 
(defun manhattan-f (path)
(+ (- (list-length path) 1) (manhattan (get-state (car path)))) 
)


; Takes an evaluation function (hueristic function) f.
; Returns a lambda function. 
; The lambda function takes two paths. 
; If path 1 is better (has a lower or equal h-cost) than path 2
;           return T otherwise return nil.
(defun better (f)
(lambda (p1 p2) (<= (funcall f p1) (funcall f p2))
))


; (((NIL (2 8 3 1 6 4 7 e 5))) (path (state)) )
; Takes a list of paths and a heuristic function.
; After every expansion sort the list of paths. 
; If a solution is found return a list of moves. 
(defun search-a* (pathLst f) 
(cond
((null pathLst) nil)
((goal-state (get-state (car (car pathLst)))) (path (car pathLst)))
(T (search-a*(sort-paths(append (cdr pathLst) (extend-path (car pathLst))) f) f) )
))

; Sorts the paths for a*-search to select the lowest h-path next. 
(defun sort-paths (paths f)
(sort paths (better f))
)

; sss - state space search 
; Takes one argument the inital state '(2 8 3 1 6 4 7 e 5).
; keyword arguments type, depth, and f.
; type determines which search algorithm to use.
; type may be 'BFS breath-first-search 
; 'DFS' depth-first-search, 'ID iterative deepening, or 'A* a*-search.
; depth is an int and determines the maximum depth for DFS.
; If type is not specified BFS will be done.
; If depth is not specified for DFS max depth is 7.
; If f is a heuristic function for A*.
; If f is not specified out-of-place-f is used by default.
; If a solution is found a list of moves to reach the solution is returned.
; If no solution is found nil is returned.  
(defun sss (lst &key type depth f)
(cond
((null type) (search-bfs (make-open-init lst)))
((equal type 'BFS) (search-bfs (make-open-init lst)))
((equal type 'DFS) 
    (cond 
    ((null depth) (search-dfs-fd (make-open-init lst) 7)) 
    (T (search-dfs-fd (make-open-init lst) depth))
    )
)
((equal type 'ID) (search-id (make-open-init lst)))
((equal type 'A*)
    (cond 
    ((null f) (search-a* (make-open-init lst) #'out-of-place-f))
    (T (search-a* (make-open-init lst) f))
    )
)
))