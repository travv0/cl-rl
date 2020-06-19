(in-package #:rl)

(defconst +letters+
  (loop for c from (char-code #\a) to (char-code #\z)
        collecting (code-char c)))
