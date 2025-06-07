(in-package #:rl/tests)
(in-suite rl)

(test pos-creation
  (with-empty-state
    (let ((pos (make-instance 'rl::pos :x 10 :y 20)))
      (is (= (rl::x pos) 10))
      (is (= (rl::y pos) 20)))
    
    ;; Test the pos function
    (let ((pos2 (rl::pos 15 25)))
      (is (= (rl::x pos2) 15))
      (is (= (rl::y pos2) 25)))))

(test pos-arithmetic
  (with-empty-state
    (let ((p1 (rl::pos 5 10))
          (p2 (rl::pos 3 4)))
      ;; Test add
      (let ((sum (rl::add p1 p2)))
        (is (= (rl::x sum) 8))
        (is (= (rl::y sum) 14)))
      
      ;; Test sub
      (let ((diff (rl::sub p1 p2)))
        (is (= (rl::x diff) 2))
        (is (= (rl::y diff) 6)))
      
      ;; Test mult
      (let ((scaled (rl::mult p1 2)))
        (is (= (rl::x scaled) 10))
        (is (= (rl::y scaled) 20)))
      
      ;; Test abs-val
      (let ((p3 (rl::pos -5 -10))
            (abs-pos (rl::abs-val (rl::pos -5 -10))))
        (is (= (rl::x abs-pos) 5))
        (is (= (rl::y abs-pos) 10))))))

(test pos-equality
  (with-empty-state
    (let ((p1 (rl::pos 5 5))
          (p2 (rl::pos 5 5))
          (p3 (rl::pos 5 6)))
      (is (rl::pos-equal p1 p2))
      (is (not (rl::pos-equal p1 p3))))))

(test distance-calculation
  (with-empty-state
    (let ((p1 (rl::pos 0 0))
          (p2 (rl::pos 3 4)))
      (is (= (rl::distance p1 p2) 5.0))
      (is (= (rl::distance p2 p1) 5.0)))))

(test to-pos-conversion
  (with-empty-state
    ;; Test with list
    (let ((pos1 (rl::to-pos '(5 10))))
      (is (= (rl::x pos1) 5))
      (is (= (rl::y pos1) 10)))
    
    ;; Test with dotted pair
    (let ((pos2 (rl::to-pos '(7 . 8))))
      (is (= (rl::x pos2) 7))
      (is (= (rl::y pos2) 8)))))

(test get-line
  (with-empty-state
    ;; Simple horizontal line
    (let* ((start (rl::pos 0 0))
           (end (rl::pos 3 0))
           (line (rl::get-line start end)))
      (is (= (length line) 4))
      (is (rl::pos-equal (first line) start))
      (is (rl::pos-equal (car (last line)) end)))
    
    ;; Simple vertical line
    (let* ((start (rl::pos 0 0))
           (end (rl::pos 0 3))
           (line (rl::get-line start end)))
      (is (= (length line) 4)))
    
    ;; Diagonal line
    (let* ((start (rl::pos 0 0))
           (end (rl::pos 3 3))
           (line (rl::get-line start end)))
      (is (>= (length line) 4)))))

(test update-pos
  (with-empty-state
    (let ((obj (make-instance 'rl::pos :x 5 :y 5)))
      (rl::add-object obj)
      (is (member obj (aref rl::*pos-cache* 5 5)))
      
      ;; Update position
      (rl::update-pos obj 7 8)
      (is (= (rl::x obj) 7))
      (is (= (rl::y obj) 8))
      (is (not (member obj (aref rl::*pos-cache* 5 5))))
      (is (member obj (aref rl::*pos-cache* 7 8))))))