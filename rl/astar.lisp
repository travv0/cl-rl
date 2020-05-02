(in-package :astar)

(defstruct node
  path
  path-length
  total-length-estimate)

(defun find-shortest-path (start goal successors heuristic-dist)
  (do (head-node		; node at head of open list
       path-to-extend         ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       (open			; list of all candidate nodes
        (list (make-node :path (list start)
                         :path-length 0
                         :total-length-estimate
                         (funcall heuristic-dist start goal))))
       (state-distances (make-hash-table :test #'equalp)))
      ((null open) nil)         ; if open list is empty, search fails
    (setq head-node (pop open))       ; get node at head of open list
    (setq path-to-extend (node-path head-node)) ; get path itself
    (setq current-state (car path-to-extend)) ; get state this path ends at
    (when (equalp current-state goal)
      (return head-node))	; success: return path and length found
    (setq dist-so-far (node-path-length head-node))
    (when (less-than dist-so-far (gethash current-state state-distances))
      (setf (gethash current-state state-distances) dist-so-far)
      (let (next-state
            next-dist-so-far)
        (dolist (pair (funcall successors current-state))
          (setq next-state (car pair))
          (setq next-dist-so-far (+ (cdr pair) dist-so-far))
          (when (less-than next-dist-so-far
                           (gethash next-state state-distances))
            (setf open
                  (merge
                   'list
                   (list
                    (make-node
                     :path (cons next-state path-to-extend)
                     :path-length next-dist-so-far
                     :total-length-estimate
                     (+ next-dist-so-far
                        (funcall heuristic-dist next-state goal))))
                   open
                   #'<
                   :key #'node-total-length-estimate))))))))

(defun less-than (x y)
  (or (null y) (< x y)))
