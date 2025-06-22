;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Jun 22 01:56:49 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "MINI-CLOG")

(defclass webgl-mixin ()
  ((width
    :initform 800
    :initarg :width)
   (height
    :initform 600
    :initarg height)
   (program
    :initform nil)
   (vao
    :initform nil)
   (gl
    :initarg :gl
    :initform nil)
   (vs-source
    :initarg :vs-source
    :initform nil)
   (fs-source
    :initarg :fs-source
    :initform nil)
   (vertex-data
    :initform '(-1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0)
    :initarg :vertex-data)
   ))

(defclass breen4-app (webgl-mixin)
  ((pos :initform nil)
   (pos-buffer :initform nil)
   (theta :initform 0.0)
   (theta-loc :initform nil))
  (:default-initargs
   :vs-source "#version 300 es
#ifdef GL_ES
precision mediump float;
#endif

in vec2 aPosition;
uniform float theta;
out vec4 vColor;
void main()
{
gl_Position.x = cos(theta) * aPosition.x - sin(theta) * aPosition.y;
gl_Position.y = sin(theta) * aPosition.x + cos(theta) * aPosition.y;
gl_Position.z = 0.0;
gl_Position.w = 1.0;
    vColor = vec4(0.0,1.0,1.0,1.0);
}"
   :fs-source
   "#version 300 es
#ifdef GL_ES
precision mediump float;
#endif
in vec4 vColor;
out vec4 fColor;

void main()
{
    fColor = vColor;
}
"
   :vertex-data
   #(-0.5 -0.5
     -0.5  0.5
     0.5 0.5
     0.5 -0.5))
  )

(defvar $app (make-instance 'breen4-app))

(defmethod init-fn ((app breen4-app))
  (with-slots (gl
	       vs-source fs-source  height width
	       vertex-data program theta-loc pos pos-buffer vao)
      app
    (setq program         (init-shaders gl vs-source fs-source))
    (setq pos             (attribute-location gl program "aPosition"))
    (setq theta-loc       (uniform-location gl program "theta"))
    (setq pos-buffer      (create-webgl-buffer gl))
    (setq vao             (create-vertex-array gl))
    (bind-buffer gl  pos-buffer :ARRAY_BUFFER)
    (buffer-data gl  pos-buffer vertex-data
		 "Float32Array" :STATIC_DRAW :ARRAY_BUFFER)
    (bind-vertex-array gl vao)
    (enable-vertex-attribute-array gl pos)
    (vertex-attribute-pointer gl pos 2 :FLOAT nil 0 0)
    (viewport gl 0 0 (min width height) (min width height))))

(defmethod draw-fn ((app breen4-app))
  (with-slots (gl program vao theta theta-loc) app
    (clear-color gl 0.0 0.0 0.0 1.0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (use-program gl program)
    (bind-vertex-array gl vao)
    (decf theta 0.01)
    (if (or (>= theta  (coerce pi 'single-float))
	    (<= theta (-  (coerce pi 'single-float))))
	(setq theta 0.0))
    ;;(gl:clear-color 0.5 0.2 0.2 1.0)
    ;;(clear-webgl gl (list :color_buffer_bit :depth_buffer_bit))
    (uniform-float gl theta-loc theta)
    (draw-arrays gl :TRIANGLE_FAN 0 4)))

(defvar $canvas nil)

(defun make-canvas ()
  (setq $canvas (create-element "canvas" "gl-canvas"))
  (assert (equal "gl-canvas" (get-attr $canvas "id")))
  (setf (get-attr "gl-canvas" "width") 400)
  (setf (get-attr "gl-canvas" "height") 400)
  (append-child "gl-canvas")
  (setf (get-prop "gl-canvas" "style" "background") "'black'")
  $canvas)

(add-to-hook '*boot-hooks* 'make-canvas)

#||
(setq websocket-server::*chat-handler*
      (clack:clackup #'websocket-server:chat-server :port 12345))
;; *boot-hooks*
;; $canvas
;; (make-canvas)
;; (call-in-ws-repl "document.getElementById('gl-canvas') === objreg['gl-canvas']")

(boot)
(setf (slot-value $app 'width) (parse-integer (get-attr "gl-canvas" "width")))
(setf (slot-value $app 'height) (parse-integer (get-attr "gl-canvas" "height")))
(slot-value $app 'theta)
(setf (slot-value $app 'gl) (create-webgl "gl-canvas" "gl"))
(init-fn $app)
(draw-fn $app)

(loop for i below 10
      do (draw-fn $app)
      (sleep 0.2))

(clack:stop websocket-server::*chat-handler*)
(call-in-ws-repl "socket.onmessage")
(call-in-ws-repl "socket.onmessage = function (event) { eval(event.data) }")
||#
