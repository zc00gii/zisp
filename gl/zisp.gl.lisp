(defpackage #:zisp.gl
  (:use :cl))

(in-package :zisp.gl)

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
      (sdl:window 100 100
		  :title-caption "OpenGL Example"
		  :icon-caption "OpenGL Example"
		  :opengl t
		  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
      (gl:clear-color 0 0 0 0)
      (gl:load-identity)
      (gl:translate 0 0 -6)
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (gl:clear :color-buffer-bit)
	       (gl:color 1 1 1)
	       (gl:with-primitive :quads
		 (flet ((v (x y z) (gl:vertex x y z)))))


	       (gl:flush)
	       (sdl:update-display))))))

(defun 3d-test-2 ()
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
    (gl:translate 3 2 -30)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (gl:clear :color-buffer-bit)
	     (gl:color 1 1 1)
	     (gl:with-primitive :polygon
	       (flet ((v (x y z) (gl:vertex x y z)))
		 (v -5 -5 4)
		 (v -5 1 4)
		 (v 1 1 4)
		 (v 1 -5 4)))
	     (gl:flush)
	     (sdl:update-display)))))