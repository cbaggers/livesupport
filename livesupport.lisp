(in-package #:livesupport)

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Livesupport: Continue")))

(macrolet
    ((impl ()
       (let ((impl (or (and (find-package :swank) :swank)
                       (and (find-package :slynk) :slynk))))
         (if impl
             `(progn
                ;;
                (defun update-repl-link ()
                  "Called from within the main loop, this keep the lisp repl
     working while cepl runs"
                  (let ((connection (or ,(intern "*EMACS-CONNECTION*" impl)
                                        (,(intern "DEFAULT-CONNECTION" impl)))))
                    (continuable
                      (when connection
                        (,(intern "HANDLE-REQUESTS" impl) connection t)))))
                ;;
                (defun peek (x)
                  (,(intern "INSPECT-IN-EMACS" impl) x)))
             `(progn
                ;;
                (defun update-repl-link ()
                  "Usually, when called from within the main loop, this keep the lisp repl
     working while cepl runs, however this is a no-op as neither
     swank nor slynk were detected"
                  (values))
                ;;
                (defun peek (x) x))))))
  (impl))
