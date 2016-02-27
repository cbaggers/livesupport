(in-package #:livesupport)

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Livesupport: Continue")))

(let (peek
      pump
      connection)
  ;;
  (defun init ()
    (let* ((swank (find-package :swank))
	   (slynk (find-package :slynk))
	   (impl (or swank slynk)))
      (setf connection (or (symbol-value (intern "*EMACS-CONNECTION*" impl))
			   (funcall (symbol-function
				     (intern "DEFAULT-CONNECTION" impl))))
	    pump (symbol-function (intern "HANDLE-REQUESTS" impl))
	    peek (symbol-function (intern "INSPECT-IN-EMACS" impl)))))
  ;;
  (defun update-repl-link ()
    "Called from within the main loop, this keep the lisp repl
     working while cepl runs"
    (continuable
      (unless connection (init))
      (when connection (funcall pump connection t))))
  ;;
  (defun peek (x)
    (unless peek (init))
    (funcall peek x)))
