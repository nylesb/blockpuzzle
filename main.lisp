;;; Data structure to store data while traverse the configuration space
(defclass node ()
  ((state :initarg :state :accessor state)
   (cost :initarg :cost :accessor cost)
   (path :initarg :path :accessor path)
   (h-estimate :initarg :h-estimate :accessor h-estimate)
   (cost-plus-h :initarg :cost-plus-h :accessor cost-plus-h)))

;(defmethod initialize-instance :after ((mynode node) &key)
;  (let ((state (slot-value mynode 'state))
;        (cost (slot-value mynode 'cost)))
;    (setf (slot-value mynode 'h-estimate) (+ cost cost))))

(defun blockpuzzle (input)
  "Implements solution to the blokpuzzle for Greedy, Uniform cost & A* search"
  (let* ((open ())
         (closed ())
         (examined-states 1)
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
                 (unless puzzle (return-from process-path nil))
                 (dolist (move (reverse path))
                   (swap-blank puzzle move)
                   (push (cons (copy-list puzzle) (if (= 0 move) "S_zero" move)) path-with-dir))
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
             (GS (input)
               "Solves the puzzle using Greedy method. When equally promising
               nodes are found, one is chosen at random (to avoid infinite loops)."
               (let ((path '(0)))
                 (do ((best-choice nil)
                      (best-h nil)
                      (puzzle (copy-list input)))
                   ((= (h puzzle) 0))
                   (setf examined-states (+ examined-states 1))
                   (if (= (h puzzle) 0) (return-from GS nil))
                   ;; Create open, being careful about edge cases
                   (setf open (subseq '(-3 -2 -1 1 2 3)
                                      (max (- 3 blank-pos) 0)
                                      (min (+ 2 (- (length puzzle) blank-pos)) 6)))
                   (setf best-choice (list (first open)))
                   (setf best-h (swap-blank puzzle (first open) :h t))
                   ;; Look for the best choice for next node, then make choice 
                   (dolist (i open)
                     (when (< (swap-blank puzzle i :h t) best-h)
                       (setf best-choice (list i))
                       (setf best-h (swap-blank puzzle i :h t)))
                     (when (= (swap-blank puzzle i :h t) best-h)
                       (setf best-choice (append best-choice (list i)))))
                   ;; Choose randomly from best options to avoid infinite loops.
                   (push (random-element best-choice) path)
                   (swap-blank puzzle (first path)))
                 (print (list examined-states (apply '+ (mapcar #'abs path)) (process-path path)))))
             (search (puzzle &key UCS ASTAR)
               "Template for solving puzzle via search using open and closed lists.
               Callable for UCS or ASTAR, which sort nodes with different priorities."
               (unless puzzle (return-from search (print "Empty puzzle")))
               (setf open (list (make-instance 'node :state puzzle :cost 0 :path '(0)
                                                     :h-estimate (h puzzle)
                                                     :cost-plus-h (h puzzle))))
               (setf count 0)
               (do* ((current-node (first open) (first open))
                     (current-h (h (state current-node)) (h (state current-node)))
                     (moves 0 0))
                 ((= current-h 0)) ; End when at goal
                 (setf examined-states (+ examined-states 1))
                 ;; Get correct position of the blank so that swap-blank works
                 (setf blank-pos (dotimes (i (length (state current-node)))
                                   (if (equal (nth i (state current-node)) 'O)
                                       (return i))))
                 ;; Determine all potential moves from current state
                 (setf moves (subseq '(-3 -2 -1 1 2 3)
                                        (max (- 3 blank-pos) 0)
                                        (min (+ 2 (- (length puzzle) blank-pos)) 6)))
                 ;; Apply move to current node and generate that on open,
                 ;; unless the node has been generated already.
                 (dolist (x moves)
                   (let* ((child-state (copy-list (swap-blank (state current-node) x :peek t)))
                          (child-cost (+ (cost current-node) (abs x)))
                          (child-path (append (list x) (path current-node)))
                          (next-node (make-instance 'node :state child-state
                                                          :cost child-cost
                                                          :path child-path)))
                     (setf (slot-value next-node 'h-estimate) (h (state next-node)))
                     (setf (slot-value next-node 'cost-plus-h) (+ (cost next-node) (h-estimate next-node)))
                     (if (not (member-list child-state closed))
                         (setf open (append open (list next-node))))))
                 ;; Turn open into a priority queue, sorting by cost
                 ;; Note: A heap would be more efficient
                 (if ASTAR (sort open (lambda (x y) (if (< (cost-plus-h x) (cost-plus-h y)) t nil))))
                 (if UCS (sort open (lambda (x y) (if (< (cost x) (cost y)) t nil))))
                 (setf closed (append closed (list (state (pop open)))))
                 ;; Clear open of any nodes we might have visted already.
                 ;; This can happen if a node generates a node already on open but not closed.
                 ;; We we always visit a node optimally though.
                 (do ((current-state (state (first open)) (state (first open))))
                   ((not (member-list current-state closed)))
                   (pop open)))
               ;; Pretty printing for UCS below, if desired.
               ;;(princ (format nil "~%Puzzle: ~A~%States examined: ~A~%Solution cost: ~A~%Path: ~A~%Result: ~A~%"
               ;;         puzzle examined-states (cost (first open)) (process-path (path (first open))) (state (first open))))))
               ;; Printing as per assignment requireents below.
               (print (list examined-states (cost (first open)) (process-path (path (first open))))))
             (UCS (puzzle)
               "See search method for implementaiton"   
               (search puzzle :UCS t))
             (ASTAR (puzzle)
               "See search method for implementation"     
               (search puzzle :ASTAR t)))
      (print "Greedy")
      (GS input)
      (setf examined-states 1)
      (setf open ())
      (setf closed ())
      (print "UCS")
      (UCS input)
      (setf examined-states 1)
      (setf open ())
      (setf closed ())
      (print "ASTAR")
      (ASTAR input)
      (terpri))))