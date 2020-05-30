(:play ((move-up (up #\k))
        (move-down (down #\j))
        (move-right (right #\l))
        (move-left (left #\h))
        (move-up-left #\y)
        (move-up-right #\u)
        (move-down-left #\b)
        (move-down-right #\n)
        (run-up (;; sup
                 #\K))
        (run-down (;; sdown
                   #\J))
        (run-right (;; sright
                    #\L))
        (run-left (;; sleft
                   #\H))
        (run-up-left #\Y)
        (run-up-right #\U)
        (run-down-left #\B)
        (run-down-right #\N)
        (wait #\.)
        (reveal-map #\R)
        (reset #\r)
        (quit #\Esc)
        (open-inventory #\i))

 :inventory ((close-inventory #\Esc)))
