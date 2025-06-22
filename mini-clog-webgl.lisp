;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Jun 22 10:05:57 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "MINI-CLOG")


;;; ----------------------------------------------------------------------
;;;
;;; WEBGL
;;;

;; probably adapted from drexel webgl coursework code
(defun init-shaders (gl-id vertString fragString &optional prog-id)
  (unless prog-id
    (setq prog-id (format nil "Gprorg~A" (generate-id))))
  (call-in-ws-repl (format nil "
var gl_id = '~A';
var prog_id = '~A';

  function createShader(gl, source, type) {
      var shader = gl.createShader(type);
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if ( !gl.getShaderParameter(shader, gl.COMPILE_STATUS) ) {
            var msg = type + \"shader failed to compile.  The error log is:\"
                + \"<pre>\" + gl.getShaderInfoLog( shader ) + \"</pre>\";
          alert( msg );
          gl.deleteShader(shader);
          return -1;
      }
      return shader;
  }

  function initShaders(gl, vertString, fragString)
  {
      if (!vertString || !fragString) {
          alert(\"Could not find \" +
		(!vertString ? \"vertex\" : \"fragment\")
		+ \" shader source\");
          return -1
      }

      var vertShdr = createShader(gl, vertString, gl.VERTEX_SHADER);
      var fragShdr = createShader(gl, fragString, gl.FRAGMENT_SHADER);
      if ((vertShdr == -1) || (fragShdr == -1)) return -1;
      var program = gl.createProgram();
      gl.attachShader( program, vertShdr );
      gl.attachShader( program, fragShdr );
      gl.linkProgram( program );

      if ( !gl.getProgramParameter(program, gl.LINK_STATUS) ) {
          var msg = \"Shader program failed to link.  The error log is:\"
              + \"<pre>\" + gl.getProgramInfoLog( program ) + \"</pre>\";
          alert( msg );
          return -1;
      }
      gl.deleteShader(vertShdr);
      gl.deleteShader(fragShdr);
      return program;
  }

check_id(prog_id);
objreg[prog_id]=initShaders(objreg[gl_id], '~A', '~A');
prog_id
"
			   gl-id prog-id
			   (websocket-server::escape-string vertString)
			   (websocket-server::escape-string fragString))))



;;; ----------------------------------------------------------------------
;;;
;;; api mostly copied and changed from clog/src/clog-webgl.lisp
;;;

(defun create-webgl (canvas-id &optional id (context "webgl2") attributes)
  (let ((context-id (or id (format nil "Ggl~A" (generate-id)))))
    (call-in-ws-repl (format nil "
var canvas_id = '~A';
var context_id = '~A';
check_id(context_id);
objreg[context_id]=objreg[canvas_id].getContext('~A'~@[,{~{~A: ~A~^,~}}~]);
context_id
"
			     canvas-id context-id context attributes))))

(defun attribute-location (gl-id program-id name)
  (call-in-ws-repl (format nil "objreg['~A'].getAttribLocation(objreg['~A'], '~A')"
			   gl-id program-id name)))

(defun uniform-location (gl-id prog-id name &optional id)
  (let ((uniform-id (or id (format nil "Guniform~A" (generate-id)))))
    (call-in-ws-repl (format nil "
gl_id = '~A';
prog_id = '~A';
uni_id = '~A';
name = '~A';
check_id(uni_id);
objreg[uni_id]=objreg['gl'].getUniformLocation(objreg[prog_id], name);
uni_id
"
			     gl-id prog-id uniform-id name))))

(defun uniform (gl-id prog-id location-id)
  (call-in-ws-repl (format nil "objreg['~A'].getUniform(objreg['~A'],objreg['~A'])"
			   gl-id prog-id location-id)))

(defun uniform-float (gl-id location-id x &optional y z w)
  (call-in-ws-repl (format nil "
gl_id = '~A';
loc_id = '~A';
objreg[gl_id].~A"
			   gl-id location-id
	(cond
          (w (format nil "uniform4fv(objreg[loc_id], new Float32Array([~F,~F,~F,~F]))"
                     (float x) (float y) (float z) (float w)))
          (z (format nil "uniform3fv(objreg[loc_id], new Float32Array([~F,~F,~F]))"
                     (float x) (float y) (float z)))
          (y (format nil "uniform2fv(objreg[loc_id], new Float32Array([~F,~F]))"
                     (float x) (float y)))
          (x (format nil "uniform1fv(objreg[loc_id], new Float32Array([~F]))"
                     (float x)))))))

(defun uniform-integer (gl-id location-id x &optional y z w)
  (call-in-ws-repl (format nil "
gl_id = '~A';
loc_id = '~A';
objreg[gl_id].~A"
			   gl-id location-id
	(cond
          (w (format nil "uniform4iv(objreg[loc_id], new Int32Array([~F,~F,~F,~F]))"
                     (float x) (float y) (float z) (float w)))
          (z (format nil "uniform3iv(objreg[loc_id], new Int32Array([~F,~F,~F]))"
                     (float x) (float y) (float z)))
          (y (format nil "uniform2iv(objreg[loc_id], new Int32Array([~F,~F]))"
                     (float x) (float y)))
          (x (format nil "uniform1iv(objreg[loc_id], new Int32Array([~F]))"
                     (float x)))))))

(defun bind-attribute-location (gl-id index name)
  (call-in-ws-repl (format nil "objreg['~A'].bindAttributeLocation(~A, ~A, '~A')"
			   gl-id index name)))

(defun bind-buffer (gl-id webgl-buffer-id glenum-target)
  "Set BIND-TYPE of buffer to either :ARRAY_BUFFER or
:ELEMENT_ARRAY_BUFFER. WebGL2 adds:
:COPY_READ_BUFFER : Buffer for copying from one buffer object to another.
:COPY_WRITE_BUFFER : Buffer for copying from one buffer object to another.
:TRANSFORM_FEEDBACK_BUFFER : Buffer for transform feedback operations.
:UNIFORM_BUFFER : Buffer used for storing uniform blocks.
:PIXEL_PACK_BUFFER : Buffer used for pixel transfer operations.
:PIXEL_UNPACK_BUFFER : Buffer used for pixel transfer operations."
  (call-in-ws-repl (format nil "
gl_id = '~A';
buf_id = '~A';
objreg[gl_id].bindBuffer(objreg[gl_id].~A,objreg[buf_id])"
			   gl-id webgl-buffer-id glenum-target)))

(defun create-webgl-buffer (gl-id &optional id)
  (unless id (setq id (format nil "Gbuf~A" (generate-id))))
  (call-in-ws-repl (format nil "
gl_id = '~A';
ret_id = '~A';
objreg[ret_id] = objreg[gl_id].createBuffer();
ret_id"
		   gl-id id)))

(defun create-vertex-array (gl-id &optional id)
  (let ((vao-id (or id (format nil "Gvao~A" (generate-id)))))
    (call-in-ws-repl (format nil "
gl_id = '~A';
vao_id = '~A';
check_id(vao_id);
objreg[vao_id] = objreg[gl_id].createVertexArray();
vao_id"
			     gl-id vao-id))))

(defun program-parameter (gl-id prog-id enum-param)
  (call-in-ws-repl (format nil "
gl_id = '~A';
prog_id = '~A';
objreg[gl_id].getProgramParameter(objreg[prog_id], objreg[gl_id].~A)"
			   gl-id prog-id enum-param)))

(defun active-attribute (gl-id prog-id index &optional id)
  (unless id (setq id (format nil "Gactiveattrib~A" (generate-id))))
  (call-in-ws-repl (format nil "
gl_id = '~A';
prog_id = '~A';
ret_id = '~A';
objreg[ret_id] = objreg[gl_id].getActiveAttrib(objreg[prog_id], ~A);
ret_id"
			   gl-id prog-id id index)))

#||
(program-parameter "gl"   "Gprorg24"	  :ACTIVE_ATTRIBUTES)
(setq $i (active-attribute "gl" "Gprorg24" 0))
(call-in-ws-repl (format nil "objreg['~A'].~A" $i "size"))
(call-in-ws-repl (format nil "objreg['~A'].~A" $i "type"))
(call-in-ws-repl (format nil "objreg['~A'].~A" $i "name"))
||#

(defun buffer-data (gl-id buf-id data-list data-type hint glenum-target)
  (call-in-ws-repl (format nil "
gl_id = '~A';
buf_id = '~A';
objreg[gl_id].bufferData(objreg[gl_id].~A, new ~A([~{~A~^,~}]), objreg[gl_id].~A)"
			   gl-id buf-id
			   glenum-target data-type (coerce data-list 'list)
			   hint)))

(defun bind-vertex-array (gl-id vao-id)
  (call-in-ws-repl (format nil "objreg['~A'].bindVertexArray(objreg['~A'])"
			       gl-id vao-id)))

(defun enable-vertex-attribute-array (gl-id attribute-location-id)
  (call-in-ws-repl (format nil "objreg['~A'].enableVertexAttribArray(objreg['~A'])"
			   gl-id attribute-location-id)))

(defun viewport (gl-id x y width height)
  (call-in-ws-repl (format nil "objreg['~A'].viewport(~A,~A,~A,~A)"
			   gl-id x y width height)))

(defun clear-color (gl-id red green blue alpha)
  (call-in-ws-repl (format nil "objreg['~A'].clearColor(~A,~A,~A,~A)"
			   gl-id
                       red green blue alpha)))

(defun clear-webgl (gl-id glenum-mask)
  "Clears buffers to preset values. GLENUM-MASK can be:
:COLOR_BUFFER_BIT
:DEPTH_BUFFER_BIT
:STENCIL_BUFFER_BIT"
  (call-in-ws-repl (format nil "
gl_id = '~A';
objreg[gl_id].clear(objreg[gl_id].~A)"
			   gl-id glenum-mask)))

(defun use-program (gl-id prog-id)
  (call-in-ws-repl (format nil "objreg['~A'].useProgram(objreg['~A'])"
			   gl-id prog-id)))

(defun draw-arrays (gl-id primitive-type offset count)
  "Renders primitives from array data. MODE can be:
:POINTS Draws a single dot.
:LINE_STRIP Draws a straight line to the next vertex.
:LINE_LOOP Draws a straight line to the next vertex, and connects the last vertex back to the first.
:LINES Draws a line between a pair of vertices.
:TRIANGLE_STRIP
:TRIANGLE_FAN
:TRIANGLES Draws a triangle for a group of three vertices."
  (call-in-ws-repl (format nil "
gl_id = '~A';
objreg[gl_id].drawArrays(objreg[gl_id].~A,~A,~A)"
			   gl-id primitive-type  offset count)))

(defun p-true-js (value)
  "Return \"true\" if VALUE t"
  (if value
      "true"
      "false"))

(defun vertex-attribute-pointer (gl-id attribute-location size type normalize stride offset)
  (call-in-ws-repl (format nil "
gl_id = '~A';
objreg[gl_id].vertexAttribPointer(~A,~A,objreg[gl_id].~A,~A,~A,~A)"
			   gl-id
			   attribute-location size type
			   (p-true-js normalize) stride offset)))

