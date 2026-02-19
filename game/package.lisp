(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
#+windows (deploy:define-library deploy::libwinpthread :dont-deploy t)

(defpackage game
  (:use :cl :framework)
  (:export #:run))
