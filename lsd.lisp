(defpackage #:lsd
  (:use #:cl)
  (:import-from #:cl-glfw3
                #:with-init-window
                #:def-key-callback
                #:get-proc-address
                #:set-key-callback
                #:set-window-should-close
                #:window-should-close-p
                #:swap-buffers
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

(defun setup-shader ()
  (let* ((vs-code '(pos))
         (fs-code '((vari:vec4 0 0 1 1)))
         (vsc (varjo:translate (varjo:make-stage :vertex '((pos :vec4)) nil '(:400) vs-code)))
         (fsc (varjo:translate (varjo:make-stage :fragment '() nil '(:400) fs-code)))
         (vs (gl:create-shader :vertex-shader))
         (fs (gl:create-shader :fragment-shader))
         (p (gl:create-program)))
    ;; (print (varjo:glsl-code vsc))
    ;; (print (varjo:glsl-code fsc))
    (gl:shader-source vs (varjo:glsl-code vsc))
    (gl:compile-shader vs)
    (print (gl:get-shader-info-log vs))
    (gl:attach-shader p vs)
    (gl:delete-shader vs)

    (gl:shader-source fs (varjo:glsl-code fsc))
    (gl:compile-shader fs)
    (gl:attach-shader p fs)
    (gl:delete-shader fs)

    (gl:bind-attrib-location p 0 "POS")
    (let ((outname (varjo:glsl-name (nth 0 (varjo:output-variables fsc)))))
      (gl:bind-frag-data-location p 0 outname))

    (gl:link-program p)
    (gl:use-program p)))

(defun show-window (context)
  (with-init-window (:title (getf context :title)
                     :width (getf context :width)
                     :height (getf context :height)
                     :context-version-major 4
                     :context-version-minor 0
                     :resizable nil)
    ;; print OpenGL version
    (format t "OpenGL version: ~a.~a.~a"
            (glfw:get-window-attribute :context-version-major)
            (glfw:get-window-attribute :context-version-minor)
            (glfw:get-window-attribute :context-revision))
    (set-key-callback 'quit-on-escape)
    (gl:clear-color 1 1 1 1)
    (setup-shader)

    (let* ((vao (gl:gen-vertex-array))
           (vbo (gl:gen-buffers 1))
           (vertex #(-0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 0.5))
           (arr (gl:alloc-gl-array :float (length vertex))))
      (gl:bind-vertex-array vao)

      (loop
        :for n :from 0 :below (length vertex)
        :do (setf (gl:glaref arr n) (elt vertex n)))

      (gl:bind-buffer :array-buffer (elt vbo 0))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:vertex-attrib-pointer 0 2 :float nil 0 0)
      (gl:enable-vertex-attrib-array 0)

      (loop
        :until (window-should-close-p)
        :do (progn
              (gl:clear :color-buffer)
              (gl:draw-arrays :triangles 0 (length vertex)))
        :do (swap-buffers)
        :do (poll-events)))))

(defun main ()
  (show-window *context*))
