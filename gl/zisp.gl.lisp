(defpackage #:zisp.gl
  (:use :cl))

(in-package :cffi)

(defvar *x* 0.525731112119133606)
(defvar *z*  0.850650808352039932)

(defun defcarray (array type)
  (declare (vector array))
  (let ((pointer (foreign-alloc type)))
    (loop for a across array for i = 0 then (incf i) do
	 (cond ((vectorp a) (setf (mem-aref pointer :pointer i)
					    (defcarray a type)))
	       (t (setf (mem-aref pointer type i)
			(convert-to-foreign a type)))))
    (values pointer)))

(in-package :zisp.gl)

(cffi:use-foreign-library %gl::opengl)

(defmacro with-c-array ((symbol value type) &body body)
  `(let ((,symbol (cffi::defcarray ,value ,type)))
     ,@body
     (cffi:foreign-free ,symbol)))

(defun free-c-array (array)
  (cffi:foreign-free array))

(defun alloc-c-array (type)
  (cffi::defcarray #() type))

(defun make-c-array (array type)
  (cffi::defcarray array type))

(defun vertex-v4 (v)
  (with-c-array (vertex v :float)
    (%gl:vertex-4fv vertex)))

(defun vertex-v3 (v)
  (with-c-array (vertex v :float)
    (%gl:vertex-3fv vertex)))

(defun vertex-v2 (v)
  (with-c-array (vertex v :float)
    (%gl:vertex-2fv vertex)))
#+ ()
(defun vertex-pointer-3 (vertices)
  (cffi:foreign-funcall "glDrawElements"
			cl-opengl-bindings:int 3
			cl-opengl-bindings:enum))

(defun vertex (x &optional (y 0.0) (z 0.0) (w 0.0))
  (cond ((vectorp x)
	 (cond ((= (length x) 4)
		(vertex-v4 x)))
	 (cond ((= (length x) 3)
		(vertex-v3 x)))
	 (cond ((= (length x) 2)
		(vertex-v2 x))))
	(t (gl:vertex x y z w))))
(defun normal (x &optional y z)
  (cond ((vectorp x)
	 (with-c-array (normal x :float)
	   (%gl:normal-3fv normal)))
	(t (gl:normal x y z))))

(defun normalize(v)
  (declare ((vector * 3) v))
  (juggler::unit-vector v))
;  (let ((d (sqrt (apply #'+
;			(loop for i across v
;			   collect (* i i))))))
;    (apply #'vector (loop for i across v
;				 collect (float (/ i d))))))

(defun normalize!(v)
  (setf v (normalize v)))

(defun normalize-cross-product (vector1 vector2)
  (normalize (juggler::cross-product vector1 vector2)))

(defun subdivide (v1 v2 v3 depth index)
  (declare ((array * 3) v1 v2 v3) (real depth index))
  (let (v12 v23 v31)
    (if (zerop depth) t)
    (setf v12 (juggler::add-vector v1 v2)
	  v23 (juggler::add-vector v2 v3)
	  v31 (juggler::add-vector v3 v1))
    (normalize! v12)
    (normalize! v23)
    (normalize! v31)
    (subdivide v1 v12 v31 (- depth 1) index)
    (subdivide v2 v23 v12 (- depth 1) index)
    (subdivide v3 v31 v23 (- depth 1) index)
    (subdivide v12 v23 v31 (- depth 1) index)
    nil))

(defun subdivide! (v1 v2 v3 depth index)
  (declare ((array * 3) v1 v2 v3) (real depth index))
  (let (v12 v23 v31)
    (if (zerop depth) (render-triangle v1 v2 v3 index))
    (setf v12 (juggler::add-vector v1 v2)
	  v23 (juggler::add-vector v2 v3)
	  v31 (juggler::add-vector v3 v1))
    (normalize! v12)
    (normalize! v23)
    (normalize! v31)
    (subdivide! v1 v12 v31 (- depth 1) index)
    (subdivide! v2 v23 v12 (- depth 1) index)
    (subdivide! v3 v31 v23 (- depth 1) index)
    (subdivide! v12 v23 v31 (- depth 1) index)))

(defun render-triangle (v1 v2 v3 i)
  (let (d1 d2)
    (setf d1 (juggler::subtract-vector v2 v1)
	  d2 (juggler::subtract-vector v2 v3))
    (gl:with-primitives :polygon
      (normal (normalize-cross-product d1 d2))
      (vertex v1)
      (vertex v2)
      (vertex v3))))

(defun render-cube (x y z length mode)
  (flet ((v (x y z) (gl:vertex x y z)))
    (gl:with-primitive mode
      (v x y z)
      (v (+ x length) y z)
      (v (+ x length) (+ y length) z)
      (v x (+ y length) z)
      (v x y z)
      (v (+ x length) y z)
      (v (+ x length) y (+ z length))
      (v x y (+ z length))
      (v x y z)
      (v x (+ y length) z)
      (v x (+ y length) (+ z length))
      (v x y (+ z length))
      (v (+ x length) y z)
      (v (+ x length) (+ y length) z)
      (v (+ x length) (+ y length) (+ z length))
      (v (+ x length) y (+ z length))
      (v x (+ y length) z)
      (v (+ x length) (+ y length) z)
      (v (+ x length) (+ y length) (+ z length))
      (v x (+ y length) (+ z length))
      (v x y (+ z length))
      (v (+ x length) y (+ z length))
      (v (+ x length) (+ y length) (+ z length))
      (v x (+ y length) (+ z length)))))

(defun render-circle (x y radius &optional (form :polygon)
		      (intervals 10000))
  (gl:with-primitive form
    (gl:vertex x y)
    (loop for angle from 0 to intervals do
	 (gl:vertex (+ (* (sin angle) radius) x)
		    (+ (* (cos angle) radius) y)))))

(defun render-square (x y length &optional (form :polygon))
  (gl:with-primitive form
    (gl:vertex x y)
    (gl:vertex (+ x length) y)
    (gl:vertex (+ x length) (+ y length))
    (gl:vertex x (+ y length))))

(defun 2d-test-1 ()
  (sdl:with-init ()
    (sdl:window 640 480
                :title-caption "OpenGL Example"
                :icon-caption "OpenGL Example"
                :opengl t
                :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl:clear-color 0 0 0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 640 480 0 0 1)
    (gl:disable :depth-test)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       (gl:clear :color-buffer-bit)
       (gl:color 1 1 1)
       (render-square 20 20 40)
       (render-circle 200 200 70)
       (gl:flush)
       (sdl:update-display)))))

(defun 3d-test-1 ()
  (let ((angle 0))
  (sdl:with-init ()
    (sdl:window 800 600
		:title-caption "OpenGL Example"
		:icon-caption "OpenGL Example"
		:opengl t
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl:viewport 0 0 800 600)
    (gl:matrix-mode :projection)
      (gl:load-identity)
    (glu:perspective 45.0 (/ 800 600) 1.0 500.0)
    (gl:matrix-mode :modelview)
      (gl:load-identity)
    (gl:clear-color 0 0 0 0)
    (gl:translate 3 2 -60)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:rotate 1 1 1 1)
	     (gl:polygon-mode :front-and-back :fill)
	     (render-cube 10 10 0 5 :quads)
	     (gl:with-primitives :polygon
	       (vertex-v #(-3.0 -3.0 0.0))
	       (vertex-v #(-6.0 -3.0 0.0))
	       (vertex-v #(-6.0 -6.0 0.0))
	       (vertex-v #(-3.0 -6.0 0.0)))
	     (gl:polygon-mode :front-and-back :line)
	     (render-cube 0 0 0 5 :quads)
	     (gl:polygon-mode :front-and-back :point)
	     (render-cube -10 -10 0 5 :quads)
	     (gl:flush)
	     (sdl:update-display))))))