(in-package :game)

;;; Scenes 

(defclass scenes ()
  ((scene-alist :initarg :scenes)))

(defun make-scenes (2d-scenes 3d-scenes)
  (make-instance 'scenes
    :scenes (list (cons :3d 3d-scenes)
		  (cons :2d 2d-scenes))))

(defun get-scene (scenes key)
  (cdr (assoc key (slot-value scenes 'scene-alist))))

(defmacro foreach-scene (scenes (scene) fn)
  (let ((scene-list (gensym)))
    `(loop for (nil . ,scene-list) in (slot-value ,scenes 'scene-alist) do
	   (loop for ,scene in ,scene-list do ,fn))))

(defmethod resize ((scenes scenes) w h)
  (foreach-scene scenes (scene) (resize scene w h)))

(defmethod update ((scenes scenes) dt)
  (foreach-scene scenes (scene) (update-scene scene dt)))

;;; 3d test scene

(defclass camera-scene (scene-3d)
  ((rotating :initform t :type boolean)))

(defmethod update-scene ((obj camera-scene) dt)
  (with-slots (cam-pos cam-target cam-fov rotating) obj
    (let* ((speed (* dt 0.8 (if (gficl:key-down :enter) 4 1)))
	   (move-speed (* 2.5 speed)) (fov-speed (* speed 1.2))
	   (fw (gficl:normalise (gficl:-vec cam-target cam-pos)))
	   (rw (gficl:normalise (gficl:cross fw +world-up+)))
	   (target (gficl:-vec cam-pos cam-target)))
      (gficl:map-keys-down
       ;; raise/lower camera
       (:space (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed +world-up+))))
       (:left-shift (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed +world-up+))))
       ;; move forward direction
       (:w (setf target (gficl:rotate-vec target speed rw)))
       (:a (setf target (gficl:rotate-vec target speed +world-up+)))
       (:s (setf target (gficl:rotate-vec target (- speed) rw)))
       (:d (setf target (gficl:rotate-vec target (- speed) +world-up+)))
       ;; move camera
       (:up (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed fw))))
       (:down (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed fw))))
       (:left (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed rw))))
       (:right (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed rw))))
       ;; zoom
       (:z (setf cam-fov (* cam-fov (- 1 fov-speed)))
	   (resize obj (gficl:window-width) (gficl:window-height)))
       (:x (setf cam-fov (* cam-fov (+ 1 fov-speed)))
	   (resize obj (gficl:window-width) (gficl:window-height)))
       ;; change light dir
       (:t
	(setf (light-dir obj) (gficl:rotate-vec (light-dir obj) speed
						(gficl:make-vec '(1 0 0)))))
       (:y
	(setf (light-dir obj) (gficl:rotate-vec (light-dir obj) speed
						(gficl:make-vec '(0 1 0))))))
      (gficl:map-keys-pressed
       ;; toggle rotate
       (:r (setf rotating (not rotating)))
       ;; change to set pos
       (:b (setf rotating nil)
	   (setf cam-pos (gficl:make-vec '(2.3 1.8 5.4)))
	   (setf target (gficl:make-vec '(6 1 3)))
	   (setf cam-fov 0.3)
	   (resize obj (gficl:window-width) (gficl:window-height))))
      
      (cond (rotating (setf cam-pos (gficl:rotate-vec cam-pos (* (- speed) 0.1) +world-up+))
		      (setf target (gficl:+vec cam-pos))))
      (setf cam-target (gficl:-vec cam-pos target)))))

;;; scene with simple 3d shapes

(defclass simple-3d-scene (camera-scene) ())

(defun make-simple-3d-scene ()
  (make-instance
   'simple-3d-scene
   :cam-pos (gficl:make-vec '(-1 3 -2))
   :cam-target (gficl:make-vec '(-1 0 -1))
   :objects
   (list
    (make-object (get-asset 'bunny) (object-matrix '(-0.5 0 -0.5) '(1 1 1))
		 :colour (gficl:make-vec '(0.2 0.8 0 1)))
    (make-object (get-asset 'plane) (object-matrix '(0 -0.38 0) '(6 1 6))
		 :colour (gficl:make-vec '(0.7 0 0 1)))
    (make-object (get-asset 'cube) (object-matrix '(-1 -0.28 -1) '(0.1 0.1 0.1)))
    (make-object (get-asset 'cube) (object-matrix '(-1.1 0 -1.5) '(0.8 0.6 0.02)))
    (make-object (get-asset 'cube) (object-matrix '(1.5 0 -1.5) '(0.8 0.6 0.1)))
    (make-object (get-asset 'cube) (object-matrix '(1.5 -0.28 -0.7) '(0.1 0.1 0.1))
		 :light t))))

;;; 2d test scene

(defclass rects-scene (scene-2d)
  ())

(defun make-rects-scene ()
  (let ((scene
	  (make-instance
	   'rects-scene
	   :cam-pos (gficl:make-vec '(0 0 0))
	   :objects
	   (list (make-2d-object
		  (make-2d-pos (gficl:make-vec '(10 10)) (gficl:make-vec '(500 500)))
		  (cons :tex (get-asset 'uv)))))))
    
    scene))

(defmethod update-scene ((scene rects-scene) dt)
  (with-slots (cam-pos objects) scene
    (let ((square (car objects)))
      (with-slots ((2d-pos pos)) square
	(with-slots (pos) 2d-pos
	  (gficl:map-keys-down
	   (:left (setf cam-pos (gficl:+vec (gficl:*vec dt (gficl:make-vec '(100 0 0))) cam-pos)))
	   (:right (setf cam-pos (gficl:+vec (gficl:*vec dt (gficl:make-vec '(-100 0 0))) cam-pos)))
	   (:a (setf pos (gficl:+vec (gficl:*vec dt (gficl:make-vec '(-100 0))) pos)))
	   (:d (setf pos (gficl:+vec (gficl:*vec dt (gficl:make-vec '(100 0))) pos)))))
	(update-2d-object-pos square 2d-pos))))
  (call-next-method))
