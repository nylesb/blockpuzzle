;;; Data structure to traverse the state space
(defclass node ()
  ((state :initarg :state :accessor state)
   (cost :initarg :cost :accessor cost)
   (path :initarg :path :accessor path)))

(defun blockpuzzle (input)
  "Implements solution to the blokpuzzle for Greedy, Uniform cost & A* search"
  (let* ((open ())
         (closed ())
         (examined-states 0)
         (blank-pos (dotimes (i (length input)) ; Index of position of blank tile
                      (if (equal (nth i input) 'O)
                        (return i)))))
    (labels ((member-list (input-list list-of-lists)
               "Checks for membership of input-list in list-of-lists."
               (dolist (item list-of-lists)
                 (if (equal item input-list)
                     (return-from member-list t)))
               nil)
             (random-element (list)
              "Returns a random element of LIST."
              (if (not (and (list) (listp list)))
                  (nth (random (1- (1+ (length list)))) list)
                  (error "Argument to get-random-element not a list or the list is empty")))
             (h (state)
               "Heuristic. Determines 'badness' by looking at how many black
               tiles are left of white tiles, counted for each white tile."
               (let ((inversions 0))
                 (dotimes (i (length state))
                   (if (equal (nth i state) 'W)
                       (dotimes (j i)
                         (if (equal (nth j state) 'B)
                             (setf inversions (+ inversions 1))))))
                 (return-from h inversions)))
             (process-path (path)
               "Takes path as list of integers, formatting it properly for print."
               (let ((path-with-dir '())
                     (puzzle (copy-list input)))
                 (print (list "mypuz: " puzzle))
                 (unless puzzle (return-from process-path nil))
                 (dolist (move (reverse path))
                   (print (list "move: " move))
                   ;(print (swap-blank puzzle move))
                   (push move path-with-dir))
                 (reverse path-with-dir)))
             (swap-blank (state relative-position &key h peek)
               "Swaps blank with the tile at the relative-position to blank.
               Key h returns h value of the proposed swap, unchanging state.
               Key peek looks at the swapped result, but doesn't change state."
               (setf blank-pos (dotimes (i (length state)) ; Index of position of blank tile
                                 (if (equal (nth i state) 'O)
                                     (return i))))
               (setf (nth blank-pos state)
                     (nth (+ blank-pos relative-position) state))
               (setf blank-pos (+ blank-pos relative-position))
               (setf (nth blank-pos state) 'O)
               (when (or h peek) ; Undo changes, return requested result
                 (let ((value (h state))
                       (state-copy (copy-list state)))
                   (swap-blank state (- relative-position))
                   (return-from swap-blank (if h value state-copy))))
               state)
             (greedy (puzzle)
               "Solves the puzzle using Greedy method. When equally promising
               nodes are found, one is chosen at random (to avoid infinite loops)."
               (do ((best-choice nil)
                    (best-h nil))
                 ((= (h puzzle) 0))
                 (if (= (h puzzle) 0) (return-from greedy nil))
                 ;; Create open, being careful about edge cases
                 (setf open (subseq '(-3 -2 -1 1 2 3)
                                    (max (- 3 blank-pos) 0)
                                    (min (+ 2 (- (length puzzle) blank-pos)) 6)))
                 (print open)
                 (setf best-choice (list (first open)))
                 (setf best-h (swap-blank puzzle (first open) :h t))
                 ;; Look for the best choice for next node, then make choice 
                 (dolist (i open)
                   (when (< (swap-blank puzzle i :h t) best-h)
                     (setf best-choice (list i))
                     (setf best-h (swap-blank puzzle i :h t)))
                   (when (= (swap-blank puzzle i :h t) best-h)
                     (setf best-choice (append best-choice (list i)))))
                 (swap-blank puzzle (random-element best-choice))))
             (UCS (puzzle)
               "Solves puzzle using uniform cost search."
               (unless puzzle (return-from UCS (print "Empty puzzle")))
               (setf open (list (make-instance 'node :state puzzle :cost 0 :path '(0))))
               ;(print (state (first open)))
               (do* ((current-node (first open) (first open))
                     (current-h (h (state current-node)) (h (state current-node)))
                     (moves 0 0))
                 ((= current-h 0)) ; End when at goal
                 (setf examined-states (+ examined-states 1))
                 ;; Get correct position of the blank so that swap-blank works
                 (setf blank-pos (dotimes (i (length (state current-node)))
                                   (if (equal (nth i (state current-node)) 'O)
                                       (return i))))
                 ;; Consider potential moves from current state
                 (setf moves (subseq '(-3 -2 -1 1 2 3)
                                        (max (- 3 blank-pos) 0)
                                        (min (+ 2 (- (length puzzle) blank-pos)) 6)))
                 ;; Put moves into open list if not already explored
                 (dolist (x moves)
                   (let* ((child-state (swap-blank (state current-node) x :peek t))
                         (child-cost (+ (cost current-node) (abs x)))
                         (child-path (append (list x) (path current-node)))
                         (next-node (make-instance 'node :state child-state :cost child-cost :path child-path)))
                     (if (not (member-list child-state closed))
                         (setf open (append open (list next-node))))))
                 ;; Turn open into a priority queue, sorting by cost
                 ;; Note: A heap would be more efficient
                 (sort open (lambda (x y) (if (< (cost x) (cost y)) t nil)))
                 (setf closed (append closed (list (state (pop open))))))
               (princ (format nil "~%Puzzle: ~A~%States examined: ~A~%Cost: ~A~%Path: ~A~%Result: ~A~%"
                        puzzle examined-states (cost (first open)) (process-path (path (first open))) (state (first open))))))
      ;(greedy input)
      (UCS input))))