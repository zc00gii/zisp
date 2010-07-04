(asdf:defsystem :zisp.gl
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu) :components
  ((:file "zisp.gl")))