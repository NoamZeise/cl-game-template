(in-package :game)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread () (program)))

(defun program ()
  (gficl:with-window
   (:title "game"
	   :resize-callback #'resize-callback
	   :opengl-version-major 4
	   :opengl-version-minor 6)
   (setup)
   (loop until (gficl:closedp)
	 do (update-step)
	 do (render))
   (cleanup)))

(defun load-assets ()
  (setup-asset-table)
  (load-model 'sphere #p"sphere.obj")
  (load-model 'cube #p"cube.obj")
  (load-model 'cone #p"cone.obj")
  (load-model 'bunny #p"bunny.obj")
  (load-model 'plane #p"plane.obj")
  (load-image 'uv #p"assets/uv.png")
  (add-asset
   'quad
   (gficl:make-vertex-data
    (gficl:make-vertex-form
     (list (gficl:make-vertex-slot 2 :float)))
    '(((0 0)) ((1 0)) ((1 1)) ((0 1)))
    '(0 3 2 2 1 0))))

(defun create-pipelines ()
  (setf *main-pipeline* (make-main-pipeline 500 500))
  (resize-callback (gficl:window-width) (gficl:window-height)))

(defun create-scenes ()
  (setf *scenes* (make-scenes (list (make-rects-scene)) (list (make-simple-3d-scene))))
  (resize *scenes* 500 500))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (create-scenes)
  (create-pipelines)
  (gl:enable :depth-test)
  ;(glfw:swap-interval 0) ; disable vsync - better for checking performance
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun cleanup-pipelines ()
  (free *main-pipeline*))
  
(defun cleanup ()
  (cleanup-pipelines)
  (cleanup-assets))

(defun resize-callback (w h)
  ;;(resize *scenes* w h)
  (resize *main-pipeline* w h))

(defun update-step ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ;; Controls
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t)))
    ;; Update Scenes
    (update *scenes* dt)
    ;; Handle Signals
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((process-watched)
	   (reload *main-pipeline*)
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (draw *main-pipeline* *scenes*)))

;;; signal running process from repl

(defun signal-quit ()
  (glfw:set-window-should-close))

(defun signal-reload ()
  "manually trigger shader reload"
  (set-all-modified))

(defun signal-fn-lambda (fn)
  (setf *signal-fn* fn))

(defmacro signal-fn (&body body)
  "call fn during next update loop"
  `(signal-fn-lambda (function (lambda () ,@body))))

(defun signal-recreate-scenes ()
  (signal-fn (create-scenes)))

(defun signal-recreate-pipelines ()
  (signal-fn (cleanup-pipelines) (create-pipelines)))

;;; global variables

(defparameter *scenes* nil)
(defparameter *main-pipeline* nil)
(defparameter *signal-fn* nil)
