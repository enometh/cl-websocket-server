;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Jun 24 19:26:07 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "MINI-CLOG")

;;; API copied and modified from clog/source/clog-canvas.lisp

;; Implementation - context2d
;; accessors - context2d
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-canvas-prop-accessor (name string)
    `(progn (defun ,name (context-id)
	      (get-prop context-id  ,string))
	    (defsetf ,name (context-id) (new-value)
	      `(setf (get-prop ,context-id ,',string) ,new-value)))))

(def-canvas-prop-accessor fill-style "fillStyle")
(def-canvas-prop-accessor canvas-filter "filter")
(def-canvas-prop-accessor font-style "font")
(def-canvas-prop-accessor global-alpha "globalAlpha")
(def-canvas-prop-accessor global-composite-operation "globalCompositeOperation")
;; reader returns lisp boolean but setter must use "true" or "false"
(def-canvas-prop-accessor image-smoothing-enabled "imageSmoothingEnabled")
(def-canvas-prop-accessor image-smoothing-quality "imageSmoothingQuality")

(deftype line-cap-type () '(member :butt :round :square))
(def-canvas-prop-accessor line-cap "lineCap")
(def-canvas-prop-accessor line-dash-offset "lineDashOffset")

(deftype line-join-type () '(member :bevel :round :miter))
(def-canvas-prop-accessor line-join "lineJoin")

(def-canvas-prop-accessor line-width "lineWidth")
(def-canvas-prop-accessor miter-limit "miterLimit")
(def-canvas-prop-accessor shadow-blur "shadowBlur")
(def-canvas-prop-accessor shadow-color "shadowColor")
(def-canvas-prop-accessor shadow-offset-x "shadowOffsetX")
(def-canvas-prop-accessor shadow-offset-y "shadowOffsetY")
(def-canvas-prop-accessor stroke-style "strokeStyle")

(deftype text-align-type () '(member :left :right :center :start :end))
(def-canvas-prop-accessor text-align "textAlign")

(deftype text-baseline-type ()
  '(member :top :hanging :middle :alphabetic :ideographic :bottom))
(def-canvas-prop-accessor text-baseline "textBaseline")
(def-canvas-prop-accessor text-dir "direction")

;; methods - context2d
(defun arc (context-id x y radius start-angle end-angle
            &key (anticlockwise nil))
  "Adds a circular arc to the current path."
  (apply #'method-call context-id "arc"
	 x y radius start-angle end-angle
	 (if anticlockwise (list anticlockwise))))

(defun arc-to (context-id x1 y1 x2 y2)
  "Adds an arc to the current path."
  (method-call context-id "arcTo" x1 y1 x2 y2))

(defun begin-path (context-id)
  "Starts a new path empting any previous points."
  (method-call context-id "beginPath"))

(defun bezier-curve-to (context-id cp1x cp1y cp2x cp2y x y)
  "Adds a cubic Bezier curve to the current path."
  (method-call context-id "bezierCurveTo"
	       cp1x cp1y cp2x cp2y x y))

(defun clear-rect (context-id x y width height)
  "Clear rectangle to transparent black"
  (method-call context-id "clearRect" x y width height))

(defun path-clip (context-id &key path2d fill-rule)
  "Clip a path."
  (apply #'method-call context-id
	 "clip"
	 (append (if path2d (list (format nil "objreg['~A']" path2d)))
		 (if fill-rule(list fill-rule)))))

(defun close-path (context-id)
  "Adds a line to start point of path."
  (method-call context-id "closePath()"))

(defvar $2d-canvas nil)

(defun make-2d-canvas ()
  (setq $2d-canvas (create-element "canvas" "2d-canvas"))
  (setf (get-prop $2d-canvas "width") 400)
  (setf (get-prop $2d-canvas "height") 400)
  (assert (equal "null" (by-id $2d-canvas)))
  ;; (delete-element $2d-canvas)
  (append-child $2d-canvas)
  $2d-canvas)

(defvar $ctx-2d nil)

#||
(boot)
;;(pop *boot-hooks*)
;;(call-in-ws-repl "socket.onmessage = function (event) { eval(event.data) }")
(make-2d-canvas)
;;(call-in-ws-repl "objreg['ctx-2d']=undefined")
(setq $ctx-2d (create-webgl $2d-canvas "ctx-2d" "2d"))
(fill-style $ctx-2d)
(setf (line-width $ctx-2d) 10)
(with-batch-transactions (:dry-run-p nil)
  (begin-path $ctx-2d)
  (arc $ctx-2d 100 75 50 0 (coerce (* pi 2) 'single-float))
  (path-stroke $ctx-2d))

#+nil
(with-batch-transactions (:dry-run-p nil)
  ;; create clipping region
  (begin-path $ctx-2d)
  (arc $ctx-2d 100 75 50 0 (coerce (* 2 Pi) 'single-float))
  (path-clip $ctx-2d)
  ;; draw stuff that gets clipped
  (setf (fill-style $ctx-2d) "'blue'")
  (fill-rect $ctx-2d 0 0 (get-prop $2d-canvas "width")
	     (get-prop $2d-canvas "height"))
  (setf (fill-style $ctx-2d) "'orange'")
  (fill-rect $ctx-2d 0 0 100 100))
||#

(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defmacro def-canvas-create-fn (lisp-fname (&rest args) doc method-name
				  &key fmt-args-munge)
    "use fmt-args-munge if you have additional &key params in ARGS. it must include
values for (cdr args)."
    (let ((opt (member '&optional args)) key)
      (assert opt)
      (setq key (member '&key opt))
      (if (not key)
	  (assert (endp (cddr opt)))
	  (assert (= (length fmt-args-munge)
		     (+ (length (ldiff (cdr args) opt))
			(length (cdr key))))))
      (let* ((id-var (cadr opt))
	     (canvas-context-id (car args))
	     (fmt-string (format nil "objreg['~~A']=objreg['~~A'].~A(~~@{~~A~~^,~~})"
				 method-name))
	     (fmt-args2 (or fmt-args-munge (ldiff (cdr args) opt))))
	`(defun ,lisp-fname ,args
	   ,doc
	   (with-id-var (,id-var)
	     (call-in-ws-repl (format nil "check_id('~A'" ,id-var))
	     (call-in-ws-repl (format nil ,fmt-string
				      ,id-var ,canvas-context-id
				      ,@fmt-args2))
	     ,id-var))))))

#+nil
(defun create-image-data (context-id width height &optional image-id)
  "Create blank image data"
  (let ((image-id (or image-id (format nil "Gimageid~A" (generate-id)))))
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].createImageData(~A,~A)"
			     image-id context-id
			     width height))
    image-id))

(def-canvas-create-fn create-image-data (context-id width height &optional image-id)
    "Create blank image data"
    "createImageData")

#+nil
(with-batch-transactions (:dry-run-p t)
  (create-image-data "foo" 10 20 "image-id"))

(def-canvas-create-fn create-conic-gradient
    (context-id start-angle x y &optional conic-gradient-id)
    "Create conic gradient"
    "createConicGradient")

(def-canvas-create-fn create-linear-gradient (context-id x0 y0 x1 y1 &optional linergradientid)
    "Create linear gradient"
    "createLinearGradient")

(def-canvas-create-fn create-radial-gradient (context-id x0 y0 r0 x1 y1 r1 &optional radial-gradient-id)
    "Create radial gradient"
    "createRadialGradient")

;; XXX?
(defun create-pattern (context-id obj-id repetition)
  (method-call context-id "createPattern"
	       (format nil "objreg['~A']" obj-id)
	       repetition))

(defun draw-image  (context-id obj-id dx dy &key dwidth dheight sWidth sHeight sx sy)
  "Draw image at dx dy optionally dwidth and dheight"
  (apply #'method-call context-id "drawImage"
	 (format nil "objreg['~A']" obj-id)
	 (append (if sx (list sx sy) (list dx dy))
		 (if sx
		     (list swidth sheight dx dy dwidth dheight)
		     (if dwidth (list dwidth dheight))))))

(defun draw-image-from-to (context-id obj-id
			   sx sy swidth sheight
			   dx dy dwidth dheight)
  "Draw image at sx sy swidth sheight to dx dy dwidth dheight"
  (method-call context-id "drawImage"
	       (format nil "objreg['~A']" obj-id)
               sx sy swidth sheight
               dx dy dwidth dheight))

(defun ellipse (context-id x y radius-x radius-y rotation
		start-angle end-angle
		&key anticlockwise)
  "Adds an elliptical arc to the current path."
  (apply #'method-call context-id "ellipse"
	 x y radius-x radius-y rotation start-angle end-angle
	 (if anticlockwise
	     (list anticlockwise))))

(defun path-fill (context-id &key path2d fill-rule)
  "Fill a path."
  (apply #'method-call context-id "fill"
	 (append (if path2d
		     (list (format nil "objreg['~A']" path2d)))
		 (if fill-rule
		     (format nil "'~A'" fill-rule)))))

(defun fill-rect (context-id x y width height)
  "Fill rectangle with current fill-color"
  (method-call context-id "fillRect" x y width height))

(defun fill-text  (context-id text x y &key (max-width nil))
  "Fill text with current fill-color"
  (apply #'method-call context-id "fillText"
	 (concatenate 'string "'" (escape-string text) "'")
	 x y
	 (if max-width
	     (list max-width))))

(def-canvas-create-fn get-image-data (context-id sx sy sw sh &optional image-data-id)
    "Get image data from context-id."
    "getImageData")

(defun get-line-dash (context-id)
  "Get  line style dash pattern, e.g. 10, 20"
  (method-call context-id "getLineDash"))

(def-canvas-create-fn get-transform (context-id &optional transform-id)
    "Get current transform matrix as matrix"
    "getTransform")

(defun is-point-in-path (context-id x y &key path2d fill-rule)
  "Returns t if point is in path or PATH2D if specfified"
  (apply #'method-call context-id
	 "isPointInPath"
	 (append (if path2d (list (format nil "objreg['~A']" path2d)))
		 (list x y)
		 (if fill-rule (list fill-rule)))))

(defmethod is-point-in-stroke (context-id x y &key path2d)
  "Returns t if point is in stroke or PATH2D if specfified"
  (apply #'method-call context-id
	  "isPointInStroke"
	  (append (if path2d (list (format nil "objreg['~A']" path2d)))
		  (list x y))))

(defun line-to (context-id x y)
  "Adds a line to the current path."
  (method-call context-id "lineTo" x y))

(def-canvas-create-fn measure-text (context-id text &optional measure-text-id)
    "Measure text returns a text-metrics id"
    "measureText")

(defun move-to (context-id x y)
  "Moves start point of path."
  (method-call context-id "moveTo" x y))

(defun put-image-data (context-id image-data x y)
  "Put image-data at x y"
  (method-call context-id "putImageData" (objreg image-data) x y))

(defun put-image-data-dirty (context-id image-data x y
                             dx dy dwidth dheight)
  "Put portion of image-data at x y"
  (method-call context-id "putImageData" (objreg image-data) x y
               dx dy dwidth dheight))

(defun quadratic-curve-to (context-id cpx cpy x y)
  "Adds a quadratic Bezier curve to the current path."
  (method-call context-id "quadraticCurveTo" cpx cpy x y))

(defun rect (context-id x y width height)
  "Adds a rectangle to the current path."
  (method-call context-id "rect" x y width height))

(defun reset-transform (context-id)
  "Restore canvas from stack"
  (method-call context-id "resetTransform"))

(defun canvas-restore (context-id)
  "Restore canvas from stack"
  (method-call context-id "restore"))

(defun rotate (context-id value)
  (method-call context-id "rotate" value))

(defun canvas-save (context-id)
  "Save canvas to stack"
  (method-call context-id "save"))

(defun scale (context-id x y)
  (method-call context-id "scale" x y))

(defun set-line-dash (context-id value)
  "Set line style dash pattern, e.g. 10, 20"
  (method-call context-id "setLineDash" value))

(defun set-transform-with-matrix (context-id matrix-id)
  "Set-Transform-With-Matrix"
  (method-call context-id "set-transform" (objreg matrix-id)))

(defun set-transform (context-id a b c d e f g)
  "Set-Transform"
  (method-call context-id "setYransform" a b c d e f g))

(defun path-stroke (context-id &key path2d)
  "Stroke a path."
  (apply #'method-call context-id "stroke"
	 (and path2d (list (objreg path2d)))))

(defun stroke-rect (context-id x y width height)
  "Fill rectangle using current stroke-style"
  (method-call context-id "strokeRect"
	       x y width height))

(defun stroke-text  (context-id text x y &key (max-width nil))
  "Stroke text with current stroke-style"
  (method-call context-id
	       "strokeText"
	       (escape-string text)
	       x y
	       (if max-width (list max-width))))


(defun transform (context-id a b c d e f)
  (method-call context-id "transform" a b c d e f))

(defun translate (context-id x y)
  (method-call context-id "translate" x y))

;; Implementation canvas-gradient

(defun gradient-add-color-stop (canvas-gradient offset color)
  (method-call canvas-gradient "addColorStop" offset color))

;; Implementation - image-data

(defun image-width (image-data-id)
  (get-prop image-data-id "width"))

(defun image-height (image-data-id)
  (get-prop image-data-id "height"))

#+TODO
(defun json-image-data (image-data-id)
  "Setf/get json image data"
  s(js-query image-data-id (format nil "JSON.stringify(~A.data)" (objreg image-data-id))))

#+TODO
(defsetf json-image-data (image-data-id) (new-value)
  (call-in-ws-repl (format nil "objreg['~A']=new ImageData(new Uint8ClampedArray(Object.values(~A)), objreg['~A'].width)"
			   image-data-id new-value image-data-id)))

;; Implementation - text-metrics

(defun text-metrics-width (text-metrics-id)
  "Width of text"
  (get-prop text-metrics-id "width"))

(defun actual-bounding-box-left (text-metrics-id)
  "Actual bounding box left"
  (get-prop text-metrics-id "actualBoundingBoxLeft"))

(defun actual-bounding-box-right (text-metrics-id)
  "Actual bounding box right"
  (get-prop text-metrics-id "actualBoundingBoxRight"))

(defun actual-bounding-box-ascent (text-metrics-id)
  "Actual bounding box ascent"
  (get-prop text-metrics-id "actualBoundingBoxAscent"))

(defun actual-bounding-box-descent (text-metrics-id)
  "Actual bounding box descent"
  (get-prop text-metrics-id "actualBoundingBoxDescent"))

(defun font-bounding-box-ascent (text-metrics-id)
  "Font bounding box ascent"
  (get-prop text-metrics-id "fontBoundingBoxAscent"))

(defun font-bounding-box-descent (text-metrics-id)
  "Font bounding box descent"
  (get-prop text-metrics-id "fontBoundingBoxDescent"))

(defun em-height-ascent (text-metrics-id)
  "'M' height ascent"
  (get-prop text-metrics-id "emHeightAscent"))

(defun em-height-descent (text-metrics-id)
  (get-prop text-metrics-id "emHeightDescent"))

(defun hanging-baseline (text-metrics-id)
  "Hanging baseline"
  (get-prop text-metrics-id "hangingBaseline"))

(defun alphabetic-baseline (text-metrics-id)
  "Alphabetic baseline"
  (get-prop text-metrics-id "alphabeticBaseline"))

(defun ideographic-baseline (text-metrics-id)
  (get-prop text-metrics-id "ideographicBaseline"))

;; Implementation - matrix
(defun create-matrix (canvas-id &optional matrix-id &key matrix)
  "Create a new Matrix. MATRIX can be
json array 6 element for 2d or 16 for 3d."
  (declare (ignore canvas-id))
  (with-id-var (matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=new DOMMatrix(~A)"
			     matrix-id
			     (if matrix
				 (if (get-objreg matrix)
				     (objreg matrix)
				     matrix)
				 "")))
    matrix-id))

;; Properties matrix
(defun matrix-is-2d (matrix-id)
  "Setf/get miter style limit"
  (get-prop matrix-id "is2d"))

(defun matrix-is-identity (matrix-id)
  "Setf/get miter style limit"
  (get-prop matrix-id "isIdentity"))

;; Methods - matrix
(defun matrix-flip-x (matrix-id &optional new-matrix-id)
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].flipX()"
			     new-matrix-id matrix-id))
    matrix-id))

(defun matrix-flip-y (matrix-id &optional new-matrix-id)
  "Return flip-y a matrix-id"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].flipY()"
                             new-matrix-id matrix-id))
    new-matrix-id))

(defun matrix-inverse (matrix-id &optional new-matrix-id)
  "Return inverse a matrix-id"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].inverse()"
			     new-matrix-id matrix-id))
    new-matrix-id))

(defun matrix-multiply (matrix-id by-matrix &optional new-matrix-id)
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].multiply(~A)"
			     new-matrix-id matrix-id by-matrix))
    new-matrix-id))

;;; DOMMatrixReadOnly.rotateAxisAngle()

(defun matrix-rotate (matrix-id angle &optional new-matrix-id)
  "Return rotate a matrix-id"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].rotate(~A)"
                             new-matrix-id matrix-id angle))
    new-matrix-id))

;;; DOMMatrixReadOnly.rotateFromVector()

#||
(format nil "~{~A~^,~}" '(1 2 3 4))
(format nil "~@{~A~^,~}" 1 2 3 4)
(format nil "~@{~@[~A~^,~]~}" 1 2 3 nil 4)
||#

(defun scale-matrix (matrix-id sx &optional sy sz ox oy oz new-matrix-id)
  "Return scale a matrix-id by sx and optionally to
sy sz ox oy oz"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].scale(~@{~@[~A~^,~]~})"
                             new-matrix-id matrix-id
			     sx sz sy ox oy oz))
    new-matrix-id))
(defun scale3d (matrix-id sx &optional sy sz ox oy oz new-matrix-id)
  "Return scale3d a matrix-id by sx and optionally to
sy sz ox oy oz"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].scale3d(~@{~@[A~^,~]~}"
                             new-matrix-id matrix-id
			     sx sy sz ox oy oz))
    new-matrix-id))

;; skewX skewY

(defun translate-matrix (matrix-id x y &optional z new-matrix-id)
  "Return translate-matrix a matrix-id by x y and optionally z"
  (with-id-var (new-matrix-id)
    (call-in-ws-repl (format nil "objreg['~A']=objreg['~A'].translate(~A,~A~A)"
			     new-matrix-id matrix-id x y
			     (if z z "")))
    new-matrix-id))

;;; ----------------------------------------------------------------------
;;;
;;; path2d
;;;

(defun create-path2d (canvas-id &optional path2d-id &key path2d)
  "Create a new Path2d. If PATH2D is supplied creates a copy."
  (declare (ignore canvas-id))
  (with-id-var (path2d-id)
    (call-in-ws-repl (format nil "objreg['~A']=new Path2D(~A)"
			     path2d-id
			     (if path2d
				 (if (get-objreg path2d)
				     (format nil "objreg['~A']" path2d)
				     path2d)
				 "")))
    path2d-id))

;; path2d Methods
(defun path2d-add-path (path2d-id path2d)
  "Add Path to this Path"
  (method-call path2d-id "addPath" (objreg path2d)))

(defun path2d-move-to (path2d-id x y)
  (method-call path2d-id "moveTo" x y))

(defun path2d-line-to (path2d-id x y)
  (method-call path2d-id "lineTo" x y))

(defun path2d-bezier-curve-to (path2d-id cp1x cp1y cp2x cp2y x y)
  (method-call path2d-id "bezierCurveTo" cp1x cp1y cp2x cp2y x y))

(defun path2d-quadratic-curve-to (path2d-id cpx cpy x y)
  (method-call path2d-id "quadraticCurveTo" cpx cpy x y))

(defun path2d-arc (path2d-id x y radius start-angle end-angle
		   &key (anticlockwise nil))
  (method-call path2d-id "arc"
               x y radius start-angle end-angle
	       (and anticlockwise (list anticlockwise))))

(defun path2d-arc-to (path2d-id x1 y1 x2 y2)
  (method-call path2d-id "arcTo" x1 y1 x2 y2))

(defun path2d-ellipse (path2d-id x y radius-x radius-y rotation
                       start-angle end-angle
                       &key (anticlockwise nil))
  (method-call path2d-id "ellipse"
               x y radius-x radius-y rotation start-angle end-angle
               (and anticlockwise (list anticlockwise))))

(defun path2d-rect (path2d-id x y width height)
  (method-call path2d-id  "rect" x y width height))

#||
(defvar $2d-canvas (create-element "canvas" "2d-canvas"))
(setf (get-attr "2d-canvas" "width") 400)
(setf (get-attr "2d-canvas" "height") 400)
(append-child "2d-canvas")
(defvar $ctx-2d (create-webgl "2d-canvas" "ctx-2d" "2d"))
(setf (fill-style $ctx-2d) "'green'")
(call-in-ws-repl "objreg['2d-canvas']")
(call-in-ws-repl "objreg['ctx-2d'].fillStyle='green'")
(call-in-ws-repl "objreg['ctx-2d'].fillRect(0,0,400,400)")
(call-in-ws-repl "objreg['ctx-2d'].fillStyle='white'")
#+nil
(with-batch-transactions (:dry-run-p t)
  (setf (global-alpha $ctx-2d) 0.8))

(defun fill-style (context-id)
  (get-prop context-id  "fillStyle"))

#+nil
(with-batch-transactions (:dry-run-p t)
  (setf (fill-style $ctx-2d) "'green'"))

(defsetf fill-style (context-id) (new-value)
  `(setf (get-prop ,context-id "fillStyle") ,new-value))
(global-composite-operation $ctx-2d)
(image-Smoothing-Enabled $ctx-2d)
(setf (image-Smoothing-Enabled $ctx-2d) "false")
(call-in-ws-repl "1 == 1")
(call-in-ws-repl 10)
||#
