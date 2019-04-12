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


(defun reset-livecoding (&optional (repl-backend (find-if #'find-package '(:slynk :swank))))
  (check-type repl-backend (or null (member :slynk :swank)))
  (cond
    (repl-backend
     (compile 'get-server-connection
              `(lambda () (or ,(intern "*EMACS-CONNECTION*" repl-backend)
                              (,(intern "DEFAULT-CONNECTION" repl-backend)))))
     (compile 'setup-lisp-repl
              (case repl-backend
                (:slynk
                 `(lambda ()
                    (let ((repl (find (,(intern "CURRENT-THREAD" repl-backend))
                                      (,(intern "CHANNELS" repl-backend))
                                      :key #',(intern "CHANNEL-THREAD" repl-backend))))
                      (when repl
                        (,(intern "SEND-PROMPT" "SLYNK-MREPL") repl)))))
                (otherwise (lambda () (values)))))

     (compile 'update-repl-link
              `(lambda ()
                 (let ((repl (get-server-connection)))
                   (continuable
                     (when repl
                       (,(intern "HANDLE-REQUESTS" repl-backend)
                        repl t))))))
     (compile 'peek `(lambda (x) (,(intern "INSPECT-IN-EMACS" repl-backend) x)))
     (compile 'find-initial-thread
              `(lambda ()
                 (or (%find-initial-thread)
                     (let ((connection (get-server-connection)))
                       (when (,(intern "SINGLETHREADED-CONNECTION-P" repl-backend)
                              connection)
                         (,(intern "CURRENT-THREAD" repl-backend)))))))
     (compile 'move-repl-thread-to-initial-thread
              (case repl-backend
                (:swank
                 `(lambda ()
                    (let ((connection (get-server-connection))
                          (repl-thread (,(intern "CURRENT-THREAD" repl-backend)))
                          (main-thread (find-initial-thread)))
                      (unless (eq repl-thread main-thread)
                        (setf (,(intern "MCONN.REPL-THREAD" repl-backend) connection)
                              main-thread)
                        (,(intern "INTERRUPT-THREAD" repl-backend)
                         main-thread
                         (lambda ()
                           (,(intern "KILL-THREAD" repl-backend) repl-thread)
                           (,(intern "WITH-BINDINGS" repl-backend)
                            ,(intern "*DEFAULT-WORKER-THREAD-BINDINGS*" repl-backend)
                            (,(intern "HANDLE-REQUESTS" repl-backend) connection)))))
                      main-thread)))
                (otherwise `(lambda () (print "Sorry, this function is not yet supported on Sly"))))))
    (t
     (setf (fdefinition 'get-server-connection) (lambda () (values)))
     (setf (fdefinition 'setup-lisp-repl) (lambda () (values)))
     (setf (fdefinition 'update-repl-link) (lambda () (values)))
     (setf (fdefinition 'peek) (lambda () (values)))
     (setf (fdefinition 'find-initial-thread) #'%find-initial-thread)
     (setf (fdefinition 'move-repl-thread-to-initial-thread) (lambda () (error "Swank not selected as backend")))))
  repl-backend)

(reset-livecoding)
