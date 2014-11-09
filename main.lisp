(defun blockpuzzle (input)
  "Implements solution to the blokpuzzle for Greedy, Uniform cost & A* search"
  (let* ((open ())
         (closed ())
         (puzzle input)
         (blank-pos (dotimes (i (length input)) ; Index of position of blank tile
                      (if (equal (nth i input) 'O)
                        (return i))))
         (goal (let ((white 0)
                     (black 0))
                 (dolist (tile puzzle)
                   (cond ((equal tile 'W) (setf white (+ white 1)))
                         ((equal tile 'B) (setf black (+ black 1)))))
                 (append (make-list white :initial-element 'W)
                         (make-list black :initial-element 'B)))))
    (labels ((random-element (list)
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
             (swap-blank (state relative-position &key h)
               "Swaps blank with the tile at the relative-position to blank.
               Key h returns h value of the proposed swap, unchanging input.
               Key state returns new state after swap, unchanging input."
               (setf (nth blank-pos state)
                     (nth (+ blank-pos relative-position) state))
               (setf blank-pos (+ blank-pos relative-position))
               (setf (nth blank-pos state) 'O)
               (when (equal h t)
                 (let ((value (h state))
                       (state-copy state))
                   (swap-blank state (- relative-position)) ; Undo changes
                   (return-from swap-blank (if h value state-copy)))))
             (greedy ()
               "Solves the puzzle using Greedy method. When equally promising
               nodes are found, one is chosen at random (to avoid loops)."
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
             (UCS ()
               "Solves puzzle using uniform cost search."
               (setf open (list (cons puzzle 10)))
               (do* ((current-node (first open))
                     (current-h (h (first current-node)) 0))
                 ((= current-h 0)) ; End when at goal
                 (print open)
                 ;; Consider potential children
                 (setf children (subseq '(-3 -2 -1 1 2 3)
                                        (max (- 3 blank-pos) 0)
                                        (min (+ 2 (- (length puzzle) blank-pos)) 6)))
                 ;; Put children onto open list
                 (dolist (x children)
                   (nconc open (list (cons "Node" (+ (cdr current-node) (abs x))))))
                 ;; Turn open into a priority queue, sorting by cost
                 ;; Note: A heap would be more efficient
                 (sort open (lambda (x y) (if (< (cdr x) (cdr y)) t nil)))
                 (setf closed (list (pop open))))
               (print open)
               (print closed)))
      (greedy)
      (print puzzle)
      (UCS)
      (print puzzle))))