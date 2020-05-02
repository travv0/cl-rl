#+lispworks (load "~/.lispworks")
(ql:quickload :slynk)
(slynk:create-server :port 5555 :dont-close t)
(loop (sleep 1))
