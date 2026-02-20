(load "framework.asd")

(require 'asdf)
(in-package :asdf-user)

(defsystem :game
	   :defsystem-depends-on (:deploy)
:build-operation "deploy-op"
	   :build-pathname "game"
	   :entry-point "game:run"
	   :depends-on (:framework)
	   :components ((:module "game"
			 :components
			 ((:file "package")
			  (:file "constants")
			  (:file "main")
			  (:file "object")
			  (:file "scenes")			  
			  (:file "pipeline")))))
