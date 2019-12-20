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

;;; shader functions

(defun make-shader (type in-args uniforms code)
  (varjo:translate (varjo:make-stage type in-args uniforms '(:400) code)))

(defun make-shader-object (shader)
  (let* ((type (intern (format nil "~a-SHADER" (symbol-name (varjo:stage-kind shader))) :keyword))
         (so (gl:create-shader type)))
    (gl:shader-source so (varjo:glsl-code shader))
    (gl:compile-shader so)
    (let ((errstr (gl:get-shader-info-log so)))
      (unless (string= errstr "")
        (error errstr)))
    so))

(defun make-program (shaders)
  (let* ((program (gl:create-program))
         (shaderobjs (loop
                       :for s :in shaders
                       :collect (let ((so (make-shader-object s)))
                                  (gl:attach-shader program so)
                                  so))))
    (values program shaderobjs)))


;; vertex functions

(defun make-gl-array (seq type)
  (let ((arr (gl:alloc-gl-array type (length seq))))
    (loop
      :for n :from 0 :below (length seq)
      :do (setf (gl:glaref arr n) (elt seq n)))
    arr))

(defmacro with-framebuffer ((&optional fbuffer) &body body)
  `(progn
     (when ,fbuffer (gl:bind-framebuffer :framebuffer fbuffer))
     ,@body
     (when ,fbuffer (gl:bind-framebuffer :framebuffer 0))))

(defun draw-vertex (type vertex)
  (let* ((vao (gl:gen-vertex-array))
         (vbo (gl:gen-buffers 1))
         (arr (make-gl-array vertex :float)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer (elt vbo 0))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:vertex-attrib-pointer 0 2 :float nil 0 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:draw-arrays type 0 (length vertex))))

;; application code

(defun setup-shader ()
  (let* ((vert (make-shader :vertex '((pos :vec4)) nil '(pos)))
         (frag (make-shader :fragment '() nil '((vari:vec4 0 0 1 1))))
         (shaders (list vert frag)))
    (multiple-value-bind (p slis)
        (make-program shaders)
      (loop
        :for iv :in (varjo:input-variables vert)
        :for n :from 0
        :do (gl:bind-attrib-location p n (varjo:glsl-name iv)))

      (loop
        :for ov :in (varjo:output-variables frag)
        :for n :from 0
        :do (gl:bind-frag-data-location p n (varjo:glsl-name ov)))

      (gl:link-program p)
      p)))

(defun render (context)
  (gl:clear :color-buffer)
  (gl:use-program (setup-shader))
  (draw-vertex :lines #(0.8 0.8 1.0 1.0 0.5 0.5))
  (draw-vertex :triangles #(0.0 0.0 0.0 0.5 0.5 0.5 0.0 0.0)))

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

    (loop
      :until (window-should-close-p)
      :do (progn
            (render context)
            (swap-buffers)
            (poll-events)))))

(defun main ()
  (show-window *context*))
