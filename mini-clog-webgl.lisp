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
  (with-id-var (prog-id)
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
			     (escape-string vertString)
			     (escape-string fragString)))))



;;; ----------------------------------------------------------------------
;;;
;;; api mostly copied and changed from clog/src/clog-webgl.lisp
;;;

(defun create-webgl (canvas-id &optional context-id (context "webgl2") attributes)
  (with-id-var (context-id)
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

(defun uniform-location (gl-id prog-id name &optional uniform-id)
  (with-id-var (uniform-id)
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

(defun create-webgl-buffer (gl-id &optional webgl-buffer-id)
  (with-id-var (webgl-buffer-id)
    (call-in-ws-repl (format nil "
gl_id = '~A';
ret_id = '~A';
objreg[ret_id] = objreg[gl_id].createBuffer();
ret_id"
			     gl-id webgl-buffer-id))))

(defun create-vertex-array (gl-id &optional vao-id)
  (with-id-var (vao-id)
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

(defun active-attribute (gl-id prog-id index &optional gl-attrib-id)
  (with-id-var (gl-attrib-id)
    (call-in-ws-repl (format nil "
gl_id = '~A';
prog_id = '~A';
ret_id = '~A';
objreg[ret_id] = objreg[gl_id].getActiveAttrib(objreg[prog_id], ~A);
ret_id"
			     gl-id prog-id gl-attrib-id index))))

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


(defun texture-parameter (gl-id glenum-target glenum-pname)
  (call-in-ws-repl (format nil "
gl=objreg['~A'];
gl.getTexParameter(gl.~A,gl.~A)" gl-id glenum-target glenum-pname)))

(defun active-texture (gl-id glenum-texture)
  (call-in-ws-repl (format nil "
gl=objreg['~A']; gl.activeTexture(gl.~A)" gl-id glenum-texture)))

(defun blend-color (gl-id red green blue alpha)
  (call-in-ws-repl (format nil "
gl=objreg['~A'];
gl.blendColor(~A,~A,~A,~A)" gl-id red green blue alpha)))

;; from jossr/boidclog
(defun texture-image-2d (gl-id glenum-target level glenum-internal-format
			 width height border glenum-format glenum-type source)
 "Specifies a two-dimensional texture image.
target:
A GLenum specifying the binding point (target) of the active texture. Possible values:

:TEXTURE_2D
A two-dimensional texture.

:TEXTURE_CUBE_MAP_POSITIVE_X
Positive X face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_X
Negative X face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_POSITIVE_Y
Positive Y face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_Y
Negative Y face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_POSITIVE_Z
Positive Z face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_Z
Negative Z face for a cube-mapped texture.

level:
A GLint specifying the level of detail. Level 0 is the base image
level and level n is the n-th mipmap reduction level.

internal-format:
A GLenum specifying the color components in the texture. Possible values:

:RGB, :RGBA, :LUMINANCE_ALPHA, :LUMINANCE, :ALPHA, :R8, :R8_SNORM, :RG8, :RG8_SNORM, :RGB8,
:RGB8_SNORM, :RGB565, :RGBA4, :RGB5_A1, :RGBA8, :RGBA8_SNORM, :RGB10_A2, :RGB10_A2UI, :SRGB8,
:SRGB8_ALPHA8, :R16F, :RG16F, :RGB16F, :RGBA16F, :R32F, :RG32F, :RGB32F, :RGBA32F, :R11F_G11F_B10F,
:RGB9_E5, :R8I, :R8UI, :R16I, :R16UI, :R32I, :R32UI, :RG8I, :RG8UI, :RG16I, :RG16UI, :RG32I,
:RG32UI, :RGB8I, :RGB8UI, :RGB16I, :RGB16UI, :RGB32I, :RGB32UI, :RGBA8I, :RGBA8UI, :RGBA16I,
:RGBA16UI, :RGBA32I, :RGBA32UI.

width:
A GLsizei specifying the width of the texture.

height:
A GLsizei specifying the height of the texture.

border:
A GLint specifying the width of the border. Must be 0.

format:
A GLenum specifying the format of the texel data. The combinations are listed in this table:
https://www.khronos.org/registry/webgl/specs/latest/2.0/#TEXTURE_TYPES_FORMATS_FROM_DOM_ELEMENTS_TABLE

type:
A GLenum specifying the data type of the texel data. Possible values:

:UNSIGNED_BYTE:
8 bits per channel for :RGBA

:UNSIGNED_SHORT_5_6_5:
5 red bits, 6 green bits, 5 blue bits.

:UNSIGNED_SHORT_4_4_4_4:
4 red bits, 4 green bits, 4 blue bits, 4 alpha bits.

:UNSIGNED_SHORT_5_5_5_1:
5 red bits, 5 green bits, 5 blue bits, 1 alpha bit.

:BYTE

:UNSIGNED_SHORT

:SHORT

:UNSIGNED_INT

:INT

:HALF_FLOAT

:FLOAT

:UNSIGNED_INT_2_10_10_10_REV

:UNSIGNED_INT_10F_11F_11F_REV

:UNSIGNED_INT_5_9_9_9_REV

:UNSIGNED_INT_24_8

:FLOAT_32_UNSIGNED_INT_24_8_REV (pixels must be null)

source:
Can be NIL, an objreg IMG id or an objreg IMAGE-DATA id."
 (call-in-ws-repl (format nil "
gl=objreg['~A'];
gl.texImage2D(gl.~A, ~D, gl.~A, ~D, ~D, ~D, gl.~A, gl.~A~:[, null~;, ~:*~objreg['~A']~])"
			  gl-id glenum-target level
			  glenum-internal-format
			  width height border
			  glenum-format
			  glenum-type
			  source)))

(defun texture-parameter-integer (gl-id glenum-target glenum-pname value)
  (call-in-ws-repl (format nil "
gl=objreg['~A'];
gl.texParameteri(gl.~A,gl.~A,~A)"
			   gl-id glenum-target glenum-pname
			   (if (symbolp value)
			       (format nil "gl.~A" value)
			       value))))

(defun texture-parameter-float (gl-id glenum-target glenum-pname value)
  (call-in-ws-repl (format nil "
gl=objreg['~A'];
gl.texParameterf(gl.~A,gl.~A,~A)"
			   gl-id glenum-target glenum-pname
			   (if (symbolp value)
			       (format nil "gl.~A" value)
			       value))))
