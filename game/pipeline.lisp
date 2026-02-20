(in-package :game)

;;; shader

(defclass pbr-shader (normals-cam-shader) ())

(defmethod reload ((s pbr-shader))
  (shader-reload-files (s (#p"standard.vs" #p"pbr.fs") :folder (shader-subfolder #p"3d/")) shader
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

;;; post scene / shader

(defclass main-post-scene (post-scene)
  ())

(defclass main-post-shader (post-shader) ())

(defmethod reload ((s main-post-shader))
  (shader-reload-files (s (#p"post.vs" #p"post.fs") :folder (shader-subfolder #p"post/")) shader
    (gl:uniformi (gficl:shader-loc shader "screen_tex") 0)))

(defmethod shader-scene-props ((s main-post-shader) (scene post-scene))
  (with-slots (shader) s
    (gl:clear-color 0 0 0 0)
    (gl:bind-framebuffer :framebuffer 0)
    (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
    (gl:disable :multisample :depth-test :cull-face)
    (gl:clear :color-buffer-bit)
    (gficl:bind-matrix shader "transform" (transform scene))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (get-post-tex scene :3d :color-attachment0))))

;;; main pipeline

(defclass main-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type main-post-scene)))

(defun make-main-pipeline (target-w target-h)
  (let ((3d-pass (make-pbr-pass))
	(2d-pass (make-2d-pass))
	(post-scene (make-instance 'main-post-scene
				   :target-width target-w
				   :target-height target-h)))
    (resize 3d-pass target-w target-h)
    (resize 2d-pass target-w target-h)
    (set-post-texs post-scene (list (cons :2d (get-textures 2d-pass))
				    (cons :3d (get-textures 3d-pass))))
    (make-instance 'main-pipeline
		   :passes (list (cons :3d 3d-pass)
				 (cons :2d 2d-pass))
		   :shaders (list (cons :post (make-instance 'main-post-shader)))
		   :post-scene post-scene)))

(defmethod resize ((pl main-pipeline) w h)
  (resize (slot-value pl 'post-scene) w h))

(defmethod draw ((pl main-pipeline) scenes)
  (draw (get-pass pl :3d) (get-scene scenes :3d))
  (draw (get-pass pl :2d) (get-scene scenes :2d))
  (draw (get-shader pl :post) (slot-value pl 'post-scene)))
