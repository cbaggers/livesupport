(in-package #:livesupport)

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

(macrolet
    ((impl ()
       (let ((impl (or (and (find-package :swank) :swank)
                       (and (find-package :slynk) :slynk))))
         (if impl
             `(progn
                ;;
                (defun get-server-connection ()
                  (or ,(intern "*EMACS-CONNECTION*" impl)
                      (,(intern "DEFAULT-CONNECTION" impl))))
                ;;
                (defun update-repl-link ()
                  "Called from within the main loop, this keep the lisp repl
     working while cepl runs"
                  (let ((connection (get-server-connection)))
                    (continuable
                      (when connection
                        (,(intern "HANDLE-REQUESTS" impl) connection t)))))
                ;;
                (defun peek (x)
                  (,(intern "INSPECT-IN-EMACS" impl) x))
                ;;
                (defun find-initial-thread ()
                  (or (%find-initial-thread)
                      (let ((connection (get-server-connection)))
                        (when (,(intern "SINGLETHREADED-CONNECTION-P" impl)
                                connection)
                          (,(intern "CURRENT-THREAD" impl))))))
                ;;
                ,(if (eq impl :swank)
                     `(defun move-repl-thread-to-initial-thread ()
                        (let ((connection (get-server-connection))
                              (repl-thread (,(intern "CURRENT-THREAD" impl)))
                              (main-thread (find-initial-thread)))
                          (unless (eq repl-thread main-thread)
                            (setf (,(intern "MCONN.REPL-THREAD" impl) connection)
                                  main-thread)
                            (,(intern "INTERRUPT-THREAD" impl)
                              main-thread
                              (lambda ()
                                (,(intern "KILL-THREAD" impl) repl-thread)
                                (,(intern "WITH-BINDINGS" impl)
                                  ,(intern "*DEFAULT-WORKER-THREAD-BINDINGS*" impl)
                                  (,(intern "HANDLE-REQUESTS" impl) connection)))))
                          main-thread))
                     `(defun move-repl-thread-to-initial-thread ()
                        (print "Sorry, this function is not yet supported on Sly")
                        nil)))
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
                  (error "Swank/Slynk are not loaded in this image")))))))
  (impl))
