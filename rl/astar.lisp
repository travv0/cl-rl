(in-package #:rl)

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

(defun wall-p (x y dir)
  (ccase dir
    (:up (some (op (typep _ 'wall)) (get-objects-at-pos (pos x (1- y)))))
    (:down (some (op (typep _ 'wall)) (get-objects-at-pos (pos x (1+ y)))))
    (:left (some (op (typep _ 'wall)) (get-objects-at-pos (pos (1- x) y))))
    (:right (some (op (typep _ 'wall)) (get-objects-at-pos (pos (1+ x) y))))))

(defmethod wall-different-p ((obj pos) prev-x prev-y)
  (with-accessors ((x x) (y y)) obj
    (or (and (/= x prev-x) (or (not (eql (wall-p x y :up)
                                         (wall-p prev-x prev-y :up)))
                               (not (eql (wall-p x y :down)
                                         (wall-p prev-x prev-y :down)))))
        (and (/= y prev-y) (or (not (eql (wall-p x y :left)
                                         (wall-p prev-x prev-y :left)))
                               (not (eql (wall-p x y :right)
                                         (wall-p prev-x prev-y :right))))))))

(defun get-heuristic (start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

(defun get-neighbors (current-pos goal-pos)
  (mapcar (lambda (c) (cons (cons (x c) (y c)) 1))
          (remove-if (lambda (p)
                       (or (not (in-bounds-p p))
                           (member-if (lambda (obj)
                                        (and (typep obj 'solid)
                                             (not (and (= (x p) (x goal-pos))
                                                       (= (y p) (y goal-pos))))
                                             (not (typep obj 'door))))
                                      (get-objects-at-pos p))))
                     (list
                      (pos (car current-pos) (1- (cdr current-pos)))
                      (pos (car current-pos) (1+ (cdr current-pos)))
                      (pos (1- (car current-pos)) (cdr current-pos))
                      (pos (1+ (car current-pos)) (cdr current-pos))
                      (pos (1- (car current-pos)) (1- (cdr current-pos)))
                      (pos (1+ (car current-pos)) (1- (cdr current-pos)))
                      (pos (1+ (car current-pos)) (1+ (cdr current-pos)))
                      (pos (1- (car current-pos)) (1+ (cdr current-pos)))))))

(defmethod find-path ((pos pos) (goal-pos pos))
  (let ((node (find-shortest-path (cons (x pos) (y pos))
                                        (cons (x goal-pos) (y goal-pos))
                                        (rcurry 'get-neighbors goal-pos)
                                        'get-heuristic)))
    (when node (reverse (node-path node)))))

(defmethod move-toward-goal ((obj moveable) (goal-pos pos))
  (when-let* ((path (find-path obj goal-pos))
              (next-pos (second path))
              (dx (- (car next-pos) (x obj)))
              (dy (- (cdr next-pos) (y obj))))
    (setf (dx obj) dx
          (dy obj) dy)
    t))
