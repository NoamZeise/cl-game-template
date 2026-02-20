(in-package :game)

;;; shader

(defclass pbr-shader (normals-cam-shader) ())

(defmethod reload ((s pbr-shader))
  (shader-reload-files (s (#p"standard.vs" #p"pbr.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj pbr-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (car (get-asset 'uv)))
  (call-next-method))

(defmethod shader-mesh-props ((obj pbr-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl (if (listp dt) (car dt) dt)))
	  (t  (gficl:bind-gl (car (get-asset 'uv)))))))

;;; colour pass

(defclass pbr-pass (pass) ())

(defun make-pbr-pass ()
  (make-instance 'pbr-pass
     :shaders (list (make-instance 'pbr-shader))
     :description
     (make-framebuffer-description
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples (gficl:msaa-samples +msaa-samples+))))

;; shader for 2d quads

(defclass 2d-shader (shader) ())

(defmethod reload ((s 2d-shader))
  (shader-reload-files (s (#p"2d.vs" #p"2d.fs") :folder (shader-subfolder #p"2d/")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((s 2d-shader) scene)
  (gl:enable :depth-test :blend :cull-face)
  (gl:front-face :cw)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)
  (call-next-method))

(defmethod shader-scene-props ((s 2d-shader) (scene scene-2d))
  (with-slots (shader) s
    (with-slots (view-projection) scene
      (gficl:bind-matrix shader "view_projection" view-projection))))

(defmethod shader-mesh-props ((s 2d-shader) props)
  (with-slots (shader) s
    (gficl:bind-vec shader "obj_colour"
		    (cdr (assoc :colour props)))
    (let ((tex (cdr (assoc :diffuse props))))
      (ecase (car tex)
	     (:tex (gficl:bind-gl (cadr tex)))
	     (:id (gl:bind-texture :texture-2d (cdr tex)))))))


;; 2d pass

(defclass 2d-pass (pass) ())

(defun make-2d-pass ()
  (make-instance
   '2d-pass
   :shaders
   (list (make-instance '2d-shader))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment)))
   :samples (gficl:msaa-samples +msaa-samples+)))

;;; main pipeline

(defclass main-pipeline (pipeline) ())

(defun make-main-pipeline ()
  (make-instance 'main-pipeline
    :passes (list (cons :3d (make-pbr-pass))
		  (cons :2d (make-2d-pass)))))

(defmethod draw ((pl main-pipeline) scenes)
  (draw (get-pass pl :3d) (get-scene scenes :3d))
  (draw (get-pass pl :2d) (get-scene scenes :2d))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :2d)) nil
   (gficl:window-width) (gficl:window-height)))
