(asdf:defsystem :zisp.gl
  :depends-on (:lispbuilder-sdl :cl-opengl :cl-glu :juggler) :components
  ((:file "zisp.gl")))