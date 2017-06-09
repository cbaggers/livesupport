(in-package #:livesupport)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *impl* (or (and (find-package :swank) :swank)
                     (and (find-package :slynk) :slynk))))

(defun %find-initial-thread ()
  (or
   #+sbcl (sb-thread:main-thread)
   ;; process == thread in these two
   #+ccl ccl::*initial-process*
   #+lispworks mp:*main-process*
   ;; https://github.com/jcbeaudoin/MKCL/blob/master/src/c/threads.d#L4229
   ;; Shows this is reliable
   #+mkcl (find "Initial" (mt:all-threads) :test #'equal
                :key #'mt:thread-name)
   ;; https://github.com/rtoy/cmucl/blob/master/src/code/multi-proc.lisp#L1530
   ;;Shows this is reliable
   #+cmucl (find "Initial" mp:*all-processes* :test #'equal
                 :key #'mp:process-name)))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Livesupport: Continue")))

(defmacro call (name &rest args)
  `(,(intern (symbol-name name) *impl*) ,@args))

(defmacro var (name)
  (intern (symbol-name name) *impl*))

(macrolet
    ((impl ()
       (if *impl*
           `(progn
              ;;
              (defun get-server-connection ()
                (or (var *emacs-connection*)
                    (call default-connection)))
              ;;
              (defun update-repl-link ()
                "Called from within the main loop, this keep the lisp repl
     working while cepl runs"
                (let ((connection (get-server-connection)))
                  (continuable
                    (when connection
                      (call handle-requests connection t)))))
              ;;
              (defun peek (x)
                (call inspect-in-emacs x))
              ;;
              (defun find-initial-thread ()
                (or (%find-initial-thread)
                    (let ((connection (get-server-connection)))
                      (when (call singlethreaded-connection-p connection)
                        (call current-thread)))))
              ;;
              (defun move-repl-thread-to-initial-thread ()
                (let ((connection (get-server-connection))
                      (main-thread (find-initial-thread)))
                  (if (and connection main-thread)
                      (let ((repl-thread
                             (call mconn.repl-thread connection)))
                        (setf (call mconn.repl-thread connection)
                              main-thread)
                        (call interrupt-thread
                              main-thread
                              (lambda ()
                                (call kill-thread repl-thread)
                                (call with-bindings
                                      (var *default-worker-thread-bindings*)
                                      (call handle-requests connection))))
                        main-thread)
                      nil))))
           `(progn
              (defun get-server-connection () nil)
              ;;
              (defun update-repl-link ()
                "Usually, when called from within the main loop, this keep the lisp repl
     working while cepl runs, however this is a no-op as neither
     swank nor slynk were detected"
                (values))
              ;;
              (defun peek (x) x)
              ;;
              (defun find-initial-thread ()
                (%find-initial-thread))
              ;;
              (defun move-repl-thread-to-initial-thread ()
                (error "Swank/Slynk are not loaded in this image"))))))
  (impl))
