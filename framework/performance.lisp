(in-package :framework)

;;; performance analyser

(defclass performance-analyser ()
  ((pipelines :initarg :pipelines)
   (scenes :initarg :scenes)
   (current-pipeline :initform nil)
   (current-scene :initform nil)
   (scene-performance :initform (make-scene-performance))
   (performance-list :initform (list))
   (finished-performance-analysis :initform nil :accessor finished)))

(defun make-performance-analyser (pipelines scenes)
  "pipelines - a list of performance-pipeline objects
scenes - a list performance-scene objects"
  (make-instance
   'performance-analyser
   :pipelines pipelines
   :scenes scenes))

(defun make-performance-pipeline (name pipeline &key (init-function (lambda ())))
  (make-instance 'performance-pipeline :name name :pipeline pipeline :init-fn init-function))

(defun make-performance-scene (name scenes &key (duration 15) (init-function (lambda ())))
  (make-instance 'performance-scene
		 :name name :scenes scenes :duration duration :init-fn init-function))

(defmethod update ((a performance-analyser) dt)
  (with-slots (current-scene scene-performance) a
    (loop for s in (scenes current-scene) do (update-scene s dt))
    (update-scene-performance scene-performance dt)
    (if (> (scene-performance-time scene-performance) (duration current-scene))
	(end-current-scene a))))

(defmethod draw ((a performance-analyser) _)
  (with-slots (current-pipeline current-scene) a
    (draw (pipeline current-pipeline) (scenes current-scene))))

(defgeneric start-performance-analyser (obj))

(defgeneric end-performance-analyser (obj))

(defgeneric end-current-scene (obj))

(defgeneric get-performance-report (obj))

(defun save-performance-data (filename performance-analyser
					&key
					(seperator "&")
					(format-string +csv-file-format-string+))
  (with-open-file (f filename :direction :output :if-exists :overwrite
		     :if-does-not-exist :create)
		  (data-table (reverse (get-performance-report performance-analyser)) f
		:seperator seperator :format-string format-string)))

;;; performance analyser implementation

(defmethod initialize-instance :after ((a performance-analyser) &key &allow-other-keys)
	   (start-performance-analyser a))

(defun update-scene-performance (scene-performance dt)
  (setf (scene-performance-time scene-performance)
	(+ dt (scene-performance-time scene-performance)))
  (setf (scene-performance-frames scene-performance)
	(cons (make-frame-performance :fps (/ 1.0 dt))
	      (scene-performance-frames scene-performance)))
  scene-performance)

(defmethod start-performance-analyser ((a performance-analyser))
	   (with-slots (scene-performance current-scene scenes current-pipeline pipelines finished-performance-analysis performance-list) a
  (setf scene-performance (make-scene-performance))
  (setf current-pipeline (car pipelines))
  (run-init-fn current-pipeline)
  (setf current-scene (car scenes))
  (setf finished-performance-analysis nil)
  (setf performance-list nil)))

(defmethod end-performance-analyser ((a performance-analyser))
   (with-slots (scene-performance current-pipeline current-scene) a
     (generate-final-performance
      scene-performance
      (name current-pipeline)
      (name current-scene))))

(defmethod end-current-scene ((a performance-analyser))
  (with-slots (performance-list scene-performance current-scene current-pipeline) a
    (let ((performance (end-performance-analyser a)))
      (format t "ran analyser: ~%(~a, ~a)~%result: ~a~%~%"
	      (name current-pipeline) (name current-scene) performance)
      (setf performance-list (cons performance performance-list)))
    (setf scene-performance (make-scene-performance))
    (destructuring-bind (s . p) (next-pipeline+scene a)
     (setf current-scene s)
     (setf current-pipeline p)
     (run-init-fn current-pipeline))))

(defun next-pipeline+scene (performance-analyser)
  (with-slots
      (scenes pipelines current-scene current-pipeline
       finished-performance-analysis)
      performance-analyser
    (loop for (scene . rest) on scenes
	  when (equalp scene current-scene)
	  return (if rest (cons (car rest) current-pipeline)
		   (cons (car scenes) (next-pipeline performance-analyser))))))

(defun next-pipeline (performance-analyser)
  (with-slots (pipelines current-pipeline finished-performance-analysis)
      performance-analyser
    (loop for (pipeline . rest) on pipelines
	  when (equal pipeline current-pipeline)
	  return (if rest (car rest)
		   (progn
		     (setf finished-performance-analysis t)
		     (car pipelines))))))

;;; performance data saving 

(defmethod get-performance-report ((a performance-analyser))
	   (slot-value a 'performance-list))

(defun table-row (performance columns &key (seperator "&"))
  (format nil
	  (concatenate 'string "~{~a~^" seperator "~}")
   (loop for c in columns collecting
	 (funcall (cdr c) performance))))

(defun data-table (performance-report stream
				       &key
				       (seperator ",")
				       (format-string
					"~{~*~}~{~a~^,~}~%~{~a~^~%~}"))
  (let ((columns '(("pipeline" . final-performance-pipeline)
		   ("scene" . final-performance-scene)
		   ("average" . final-performance-avg-fps)
		   ("1\\% low" . final-performance-1%-low)
		   ("peak fps" . final-performance-peak-fps)
		   ("runtime" . final-performance-runtime)
		   ("resolution" . final-performance-resolution)
		   )))
    (format stream
	    format-string
	    (cdr columns)
	    (loop for c in columns collecting (car c))
	    (loop for performance in performance-report
		  collecting (table-row performance columns :seperator seperator)))))

;;; performance objects

(defclass performance-object ()
  ((name :initarg :name :type string :accessor name)
   (init-fn :initarg :init-fn :type function)))

(defclass performance-pipeline (performance-object)
  ((pipeline :initarg :pipeline :type pipeline :accessor pipeline)))

(defclass performance-scene (performance-object)
  ((scenes :initarg :scenes :type list :accessor scenes)
   (duration :initarg :duration :type real :accessor duration)))

;; helpers

(defgeneric run-init-fn (obj))

(defmethod run-init-fn ((obj performance-object))
  (funcall (slot-value obj 'init-fn)))

;;; performance structs

(defstruct frame-performance (fps 0.0 :type real))

(defstruct scene-performance
   (frames nil :type list)
   (time 0.0 :type real))

(defstruct final-performance
   (pipeline "" :type string)
   (scene "" :type string)
   (avg-fps 0.0 :type real)
   (1%-low 0.0 :type real)
   (peak-fps 0.0 :type real)
   (runtime 0.0 :type real)
   (resolution (cons 0 0)))

(defconstant +skip-frames+ 25)

(defun generate-final-performance (scene-performance pipeline-name scene-name)
  (let* ((final (make-final-performance))
	 (frames (nthcdr +skip-frames+ (scene-performance-frames scene-performance)))
	 (frame-count (length frames))
	 (1% (ceiling (/ frame-count 100)))
	 (lows (subseq (sort frames (lambda (a b) (< (frame-performance-fps a)
						     (frame-performance-fps b))))
		       0 1%)))
    (if (> 1% 0)
	(setf (final-performance-1%-low final)
	      (/ (loop for f in lows summing (frame-performance-fps f)) 1%)))
    (if (> frame-count 0)
	(setf (final-performance-avg-fps final)
	      (/ (loop for f in frames summing
		       (let ((fps (frame-performance-fps f)))
			 (if (> fps (final-performance-peak-fps final))
			     (setf (final-performance-peak-fps final)
				   fps))
			 fps))
		 (length frames))))
    (setf (final-performance-runtime final)
	  (scene-performance-time scene-performance))
    (setf (final-performance-pipeline final) pipeline-name)
    (setf (final-performance-scene final) scene-name)
    (setf (final-performance-resolution final)
	  (cons (gficl:window-width) (gficl:window-height)))
    final))

;;; performance data templates

(alexandria:define-constant +csv-file-format-string+
			    "~{~*~}~{~a~^,~}~%~{~a~^~%~}"
			    :test #'string=)

(alexandria:define-constant +latex-table-format-string+
	     "\\begin{center}
\\begin{tabular}{| c |~{ c~*~} |}
\\hline
% headers
~{ ~a & ~}
\\hline
% table data
~{ ~a \\\\~^~%~}
\\hline
\\end{tabular}
\\end{center}"
	     :test #'string=)
