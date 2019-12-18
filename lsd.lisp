(defpackage #:lsd
  (:use #:cl)
  (:import-from #:cl-glfw3
                #:with-init-window
                #:def-key-callback
                #:set-key-callback
                #:set-window-should-close
                #:window-should-close-p
                #:poll-events)
  (:export #:main))
(in-package #:lsd)

(defparameter *context*
  (list :title "Lazy Sweet Dream"
        :version (asdf:component-version (asdf:find-system :lsd))
        :width 800
        :height 600))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun show-window (context)
  (with-init-window (:title (getf context :title)
                            :width (getf context :width)
                            :height (getf context :height))
    (set-key-callback 'quit-on-escape)
    (loop
      :until (window-should-close-p)
      :do (poll-events))))

(defun main ()
  (show-window *context*))
