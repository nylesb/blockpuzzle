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
    (labels ((h (state)
               "Heuristic. Determines 'badness' by looking at how many black
               tiles are left of white tiles, counted for each white tile."
               (let ((inversions 0))
                 (dotimes (i (length state))
                   (if (equal (nth i state) 'W)
                       (dotimes (j i)
                         (if (equal (nth j state) 'B)
                             (setf inversions (+ inversions 1))))))
                 (return-from h inversions)))
             (slide-left (position)
               "Moves tile at position left into an adjacent open spot."
               (setf (nth (- position 1) puzzle) (nth position puzzle))
               (setf (nth position puzzle) 'O)
               (setf blank-pos position))
             (slide-right (position)
               "Moves tile at position right into an adjacent open spot."
               (setf (nth (+ position 1) puzzle) (nth position puzzle))
               (setf (nth position puzzle) 'O)
               (setf blank-pos position))
             (hop-left (position number)
               "Hops tile at position over number tiles into an open spot."
               (setf (nth (- position (+ number 1)) puzzle) (nth position puzzle))
               (setf (nth position puzzle) 'O)
               (setf blank-pos position))
             (hop-right (position number)
               "Hops tile at position over number tiles into an open spot."
               (setf (nth (+ position (+ number 1)) puzzle) (nth position puzzle))
               (setf (nth position puzzle) 'O)
               (setf blank-pos position))
             (greedy ()
               "Solves the puzzle using Greedy method."
               (let ((best nil))
                 (loop while (> (h puzzle) 0) do
                   (hop-left (+ blank-pos 2) 1)))))
      (greedy)
      (print puzzle))))