;;
;; A Simulator Based on Vacuum World
;; Ryan Kaulakis 2010-12-29
;; rmk216@ist.psu.edu
;; Version 0.1a
;;

(defpackage :vacworld
  (:use :common-lisp :cl-user)
  (:export :run :manual-control-loop))

(in-package :vacworld)

(defclass tile ()
  ((position
    :initform '()
    :accessor position-of
    :documentation "Location on board of tile")
   (dirtiness 
    :initform 0.0
    :accessor dirtiness
    :documentation "Degree that tile contains dirt, 0 is none, 1 is complete")))
   

(defclass obstacle (tile)
  ((blockage
    :initform 1.0
    :accessor blockage
    :documentation "Degree that obstacle prevents movement")))

(defclass io-tile (tile)
  ((owner
    :initform nil
    :accessor owner
    :documentation "Backreference to tile of owner")
   (io-type 
    :initform 'read-only
    :accessor io-type
    :documentation "Direction that I/O is performed as symbol: 'read-only 'write-only 'read-write")
   (io-delegate
    :initform nil
    :accessor io-delegate
    :documentation "Reference to the object that does the I/O on the state")))

(defclass agent (tile)
  ((agent-delegate 
    :initarg :delegate
    :initform nil
    :accessor agent-delegate
    :documentation "Reference to the object that is the 'brain' of the agent")
   (sensors 
    :initform '()
    :accessor sensors
    :documentation "List of references to io-tiles that count as sensors")
   (actuators
    :initform '()
    :accessor actuators
    :documentation "List of references to io-tiles that count as actuators")))

(defclass board ()
  ((size
    :initarg :size
    :initform nil
    :accessor size
    :documentation "Size of the board")
   (state
    :accessor state
    :documentation "current state")))

(defclass simulator ()
  ((time
    :initarg :initial-time
    :initform 0
    :accessor simulation-time
    :documentation "Current time of the simulation")
   (simulator-state
    :initarg :initial-state
    :accessor simulator-state
    :documentation "Current state of simulation")))
   


(defun fresh-state (dimen-list)
  "Takes a list containing positive integers to make the board size"
  (make-array dimen-list :element-type 'tile :initial-element (make-instance 'tile) :adjustable nil))

(defmethod initialize-instance :after ((board-instance board) &key)
  "initialize board to correct size after size supplied"
  (when (size board-instance)
    (setf (state board-instance) (funcall #'fresh-state (size board-instance)))))


(defgeneric tabular-pprint (item)
  (:documentation "Pretty-print items"))

(defmethod tabular-pprint ((item tile))
  (if (zerop (dirtiness item))
      "0";" "
      "D"))

(defmethod tabular-pprint ((item obstacle))
  "X")

(defmethod tabular-pprint ((item io-tile))
  "?")

(defmethod tabular-pprint ((item agent))
  "A")

(defun take! (n lis &optional (acc '()))
  (if (zerop n)
      (nreverse acc)
      (let ((x (pop lis)))
	(take! (- n 1) lis (cons x acc)))))
(defun take (n lis)
  (take! n (copy-tree lis)))

(defun partition! (item n &optional (acc '()))
  (if (<= (length item) n)
      (nreverse (if item (cons item acc) acc))
      (partition! (nthcdr n item) n (cons (take n item) acc))))

(defmethod tabular-pprint ((item board))
  (let ((s (state item)))
    (destructuring-bind (y x) (array-dimensions s)
      ;(format t "x is ~a, y is ~a~%" x y)
      (let* ((my-x (- x 1))
	     (my-y (- y 1))
	     (my-list '())
	     (s-list (loop for i from 0 to my-y do
			(loop for j from 0 to my-x do
			     (push (tabular-pprint (aref s i j)) my-list)))))
	;(format t "slist:~%~a~%" s-list)
	;(format t "my-list:~%~a~%" my-list)
	(setf s-list (reverse my-list))
	(let ((chunked (partition! s-list x)))
	  ;(format t "chunked:~%~a~%" chunked)
	  (let ((new-lined 
		 (mapcar #'(lambda (q) 
			     (apply #'concatenate 
				    'string 
				    (append q 
					    (list (string #\Newline)))))
			 chunked)))
	    ;(format t "new-lined:~%~a~%" new-lined)
	    (apply #'concatenate 'string new-lined)))))))

(defmethod tabular-pprint ((item simulator))
  (format nil "time=~a~%---~%~a---~%" 
	  (simulation-time item) 
	  (tabular-pprint (simulator-state item))))
	  
(defmacro aref-coordinates (obj coord-list)
  `(apply #'aref ,obj ,coord-list))

(defgeneric blocks? (item)
  (:documentation "Returns (0,1] if blocks, or nil"))
(defmethod blocks? ((item tile))
  nil)
(defmethod blocks? ((item obstacle))
  (blockage item))



(defgeneric set-tile (sim coordinate-list tile)
  (:documentation "Set contents of simulator at coordinate "))
	     

(defmethod set-tile ((sim simulator) coord-list (tile tile))
  ;(setf (apply #'aref (state (simulator-state sim)) coord-list) tile)
  (setf (aref-coordinates (state (simulator-state sim)) coord-list) tile)
  (setf (position-of tile) coord-list))


(defgeneric get-tile (sim coord-list)
  (:documentation "gets a tile from simulator"))

(defmethod get-tile ((sim simulator) coord-list)
  (aref-coordinates (state (simulator-state sim)) coord-list))
 
(defgeneric move-tile (sim obj coord-list replacement-tile)
  (:documentation "Move a tile to some coordinate, replacing the old position with a new tile, return replaced tile"))

(defmethod move-tile ((sim simulator) (tile-obj tile) coord-list replacement-tile)
  "Unconditionally moves tile"
  (let ((old-tile (get-tile sim coord-list))
	(old-pos (position-of tile-obj)))
    (set-tile sim coord-list tile-obj)
    (set-tile sim old-pos replacement-tile)
    old-tile))

(defmethod move-tile ((sim simulator) (old-pos cons) coord-list replacement-tile)
  "Convenience delegate for by-position lookup"
  (move-tile sim (get-tile sim old-pos) coord-list replacement-tile))

(defgeneric try-move (sim tile coord-list replacement-tile)
  (:documentation "returns t if nothing prevents the move"))

(defmethod try-move ((sim simulator) (tile tile) coord-list (replacement tile))
  "checks the move"
  (format t "Are coordinates ~a in bounds of ~a? ~a.~%" coord-list 
	  (array-dimensions (state (simulator-state sim)))
	  (apply #'array-in-bounds-p (state (simulator-state sim)) coord-list))
  (and (apply #'array-in-bounds-p (state (simulator-state sim)) coord-list)
       (let ((target (get-tile sim coord-list)))
	 (not (blocks? target)))))

(defparameter *directions* (pairlis '(:n :s :e :w) '((-1 0) (1 0) (0 1) (0 -1))))

(defun translate-relative (sim item direction distance)
  "get the new coordinates"
  (let* ((x (cdr (assoc direction *directions*)))
	 (vdist (mapcar #'(lambda (n) (* n distance))  x))
	 (origin (position-of item))
	 (final (mapcar #'+ origin vdist)))
    (format t "vdist is:~a~%" vdist )   
    (format t "Origin is:~a~%" origin )  
    (format t "Translated target is:~a~%" final )
    final))

(defun find-agent (sim)
  (let ((s (state (simulator-state sim))))
    (block outer
      (dotimes (i (array-dimension s 0))
	(dotimes (j (array-dimension s 1))
	  (let ((x (aref s i j)))
	  (when (eql (type-of x) 'agent)
	    (format t "found it!")
	    (return-from outer x)))))
      nil)))
    

	 
    
 
(defgeneric request-move-agent (sim direction distance)
  (:documentation "Returns multiple values, first being a boolean success value. Directions are :n, :s, :e, :w"))

(defmethod request-move-agent ((sim simulator) (direction symbol) distance)
  "basic version"
  (let* ((a (find-agent sim))
	 (empty (make-instance 'tile))
	 (dest (translate-relative sim a direction distance))
	 (can-move (try-move sim a dest empty)))
    (if can-move
	(progn
	  (move-tile sim a dest empty)
	  (values can-move))
	(progn
	  ;;return an out of bounds thing to maybe?
	  (values nil can-move)))))


(defun run ()
  "Runs the test system"
  (let* ((board (make-instance 'board :size '(5 9)))
	 (sim (make-instance 'simulator :initial-state board)))
    ;(setf (aref (state board) 0 0) (make-instance 'agent))
    ;(setf (aref (state board) 2 2) (make-instance 'obstacle))
    (set-tile sim '(0 0) (make-instance 'agent))
    (set-tile sim '(2 2) (make-instance 'obstacle))
    (format t "~a~%" (tabular-pprint sim))
    (move-tile sim '(0 0) '(0 1) (make-instance 'obstacle))
    (format t "moved agent right:~%~a~%" (tabular-pprint sim))
    (format t "move request for south2 returned ~a~%"(request-move-agent sim :s 2))
    (format t "moved agent south by 2:~%~a~%" (tabular-pprint sim))
    (format t "move request east1 returned ~a~%" (request-move-agent sim :e 1))
    (format t "moved agent east by 1:~%~a~%" (tabular-pprint sim))
    
    sim))
    ;(format t "~a~%" (state board))
    ;(format t "~a~%" (tabular-pprint board))))
    
(defun make-test-sim ()
  "just make a board that isn't empty"
  (let* ((board (make-instance 'board :size '(5 9)))
	 (sim (make-instance 'simulator :initial-state board)))
    (set-tile sim '(0 0) (make-instance 'agent))
    (set-tile sim '(2 2) (make-instance 'obstacle))
    (move-tile sim '(0 0) '(0 1) (make-instance 'obstacle))
    sim))

(defun manual-control-loop (&optional (current-sim (make-test-sim)) (halt nil))
  (let* ((escape-symbol :q))
    (unless halt
      (format t "Current state is:~%~a~%[:n,:s,:e,:w or :q to stop]> " (tabular-pprint current-sim))
      (let ((val (eval (read))))
	(if (or (eql escape-symbol val)
		(notany #'(lambda (x) (eql x val)) '(:n :e :s :w)))
	    (manual-control-loop current-sim t)
	    (let ((worked (request-move-agent current-sim val 1)))
	      (if worked
		  (format t "Success~%")
		  (format t "Failure: '~a'.~%" worked))
	      (manual-control-loop current-sim nil)))))
    current-sim))



	
	 