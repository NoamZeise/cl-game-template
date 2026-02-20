(in-package :game)

;;; 2d objects

(defclass 2d-pos ()
  ((pos :initarg :pos :type gficl:vec)
   (size :initarg :size :type gficl:vec)
   (depth :initarg :depth :type float)
   (rotate :initarg :rotate :type float)))

(defun make-2d-pos (pos size &key (depth 0) (rotate 0))
  (make-instance
   '2d-pos
   :pos pos
   :size size
   :depth depth
   :rotate rotate))

(defun get-2d-pos-matrix (2d-pos)
  (with-slots (pos size depth rotate) 2d-pos
    (let ((x (gficl:vec-ref pos 0))
	  (y (gficl:vec-ref pos 1))
	  (w (gficl:vec-ref size 0))
	  (h (gficl:vec-ref size 1)))
      (gficl:2d-rect-matrix (gficl:make-vec (list x y w h))))))

;; 2d object

(defclass 2d-object (object)
  ((pos :initarg :pos :type 2d-pos)
   (colour :initarg :colour :type gficl:vec)))

(defun make-2d-object (pos tex &key (colour (gficl:make-vec '(1 1 1 1))))
  (make-instance
   '2d-object
   :meshes (list (get-asset 'quad))
   :diffuse (list tex)
   :model (get-2d-pos-matrix pos)
   :pos pos
   :colour colour))

(defun update-2d-object-pos (2d-obj new-pos)
  (with-slots (pos model) 2d-obj
    (setf pos new-pos)
    (setf model (get-2d-pos-matrix pos))))
