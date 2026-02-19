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
  (load-model+texs 'street #p"street/street.obj")
  (load-image 'uv #p"assets/uv.png")
  (load-image 'colours #p"assets/colours.png"))

(defun create-pipelines ()
  (setf *pipelines*
	(list
	 (cons "pbr" (make-pbr-pipeline))))
  (if (not *active-pipeline*) (setf *active-pipeline* (caar *pipelines*)))
  (resize-callback (gficl:window-width) (gficl:window-height)))

(defun create-scenes ()
  (setf *scenes*
	(list ;(cons "basic" (list :scenes (list (make-simple-3d-scene)) :duration 15.0))
	      (cons "street" (list :scenes (list (make-street-scene)) :duration 30.0))))
  (setf *active-scene* (getf (cdr (assoc "street" *scenes*)) :scenes)))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (setf *active-pipeline* nil)
  (create-scenes)
  (create-pipelines)
  (gl:enable :depth-test)
  (glfw:swap-interval 0) ; disable vsync - better for checking performance
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun cleanup-pipelines ()
  (foreach-al *pipelines* (p) (if (not (reused-pipeline p)) (free (get-pipeline p)))))
  
(defun cleanup ()
  (cleanup-pipelines)
  (cleanup-assets))

(defun resize-callback (w h)
  (loop for scene in *active-scene* do (resize scene w h))
  (foreach-al *pipelines* (p) (resize (get-pipeline p) w h)))

(defun update-step ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ;; Controls
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t))
     (:m (setf *active-pipeline* (switch-active-pipeline *active-pipeline* *pipelines*))))
    ;; Update Scenes
    (loop for scene in *active-scene* do
	  (update-scene scene dt))
    ;; Handle Signals
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((process-watched)
	   (foreach-al *pipelines* (p) (reload (get-pipeline p)))
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (draw (get-pipeline (cdr (assoc *active-pipeline* *pipelines*))) *active-scene*)))

;;; update helpers

(defun switch-active-pipeline (active pipelines)
  (setf active
	(loop for ((k . _) . r) on pipelines
	      when (equalp k active)
	      return (if r (caar r) (caar pipelines))))     
  (format t "using ~a pipeline~%" active)
  (init-pipeline (assoc active pipelines))
  active)

(defun get-pipeline (p)
  (if (consp p) (car p) p))

(defun reused-pipeline (p)
  (if (consp p) (getf (cdr p) :reused) nil))

(defun init-pipeline (p)
  (if (consp (cdr p))
      (let ((fn (getf (cddr p) :reused))) (if fn (funcall fn)))))

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
(defparameter *active-scene* nil)

(defparameter *pipelines* nil)
(defparameter *active-pipeline* nil)

(defparameter *analyser* nil)

(defparameter *analyser-running?* nil)

(defparameter *signal-fn* nil)
