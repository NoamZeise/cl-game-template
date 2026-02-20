;; assume https://github.com/NoamZeise/gficl is cloned in this folder
(load "gficl/gficl.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :framework
  :depends-on (:gficl
	       :gficl/load
	       :alexandria
	       :trivial-main-thread
	       :file-notify
	       :harmony
	       ;; drain extensions for supporting different audio formats
	       :cl-mixed-mpg123
	       #+linux :cl-mixed-pulse
	       #+windows :cl-mixed-wasapi)
  :components ((:module "framework"
		:components
		((:file "package")
		 (:file "constants")
		 (:file "generic")
		 (:file "assets")
		 (:file "object")
		 (:file "scene")
		 (:file "shader")
		 (:file "pass")
		 (:file "pipeline")
		 (:file "hot-reload")
		 (:file "performance")))))
