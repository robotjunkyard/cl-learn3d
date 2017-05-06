;;;; cl-learn3d.asd

(asdf:defsystem #:cl-learn3d
  :description "Sandbox project for learning/demonstrating 3D rendering concepts, particularly aiming to hit those specific areas which tend to get abstracted-away by modern graphic APIs like OpenGL and DirectX."
  :author "njb@robotjunkyard.org"
  :license "MIT"
  :depends-on (#:split-sequence  ;; used in parsing .obj file
	       #:sdl2            ;; Simple Direct Media Layer V2
	       #:sb-cga)         ;; Nikodemus Siivola's comp. graph. arith. lib
  :serial t
  :components ((:file "package")
	       (:file "eztypes")
	       (:file "misc")
	       (:file "camera")
	       (:file "draw")
	       (:file "model")
               (:file "cl-learn3d")))
