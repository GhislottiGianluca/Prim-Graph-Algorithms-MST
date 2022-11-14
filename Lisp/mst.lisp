(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *index* (make-hash-table :test #'equal))
(defparameter *neighbours* (make-hash-table :test #'equal))
(defparameter *nodes* (make-hash-table :test #'equal))
(defparameter *adjacent*(make-hash-table :test #'equal))



(defun is-graph (graph-id)
  (if(gethash graph-id *graphs*)
      graph-id
    nil))



(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))



(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (maphash #' (lambda (k v)
		(when (equal (first k) graph-id)
		  (remhash k *neighbours*)))
	      *neighbours*)
  (maphash #' (lambda (k v)
		(when (equal (first k) graph-id)
		  (remhash k *vertex-keys*)))
	      *vertex-keys*)
  (maphash #' (lambda (k v)
		(when (equal (first k) graph-id)
		  (remhash k *previous*)))
	      *previous*)
  (maphash #'(lambda (k v)
	       (when (equal (second k) graph-id)
                 (remhash k *vertices*)))
	   *vertices*)
  (maphash #'(lambda (k v)
	       (when (equal (second k) graph-id)
                 (remhash k *arcs*)))
	   *arcs*))



(defun new-vertex (graph-id vertex-id)
  (or (gethash (list 'vertex graph-id vertex-id) *vertices*)
      (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
            (list 'vertex graph-id vertex-id))))



(defun graph-vertices (graph-id)
  (if (gethash graph-id *graphs*)
      (let((l '()))
	(maphash #'(lambda (k v)
		     (when(equal (second k) graph-id)
		       (push k l)))
		 *vertices*)
	l)
    nil))



(defun new-arc (graph-id vertex-id vertexx-id &optional(w 1))
  (new-graph  graph-id)
  (new-vertex graph-id vertex-id)
  (new-vertex graph-id vertexx-id)
  (if (not (gethash (list graph-id vertex-id) *neighbours*))
      (setf (gethash (list graph-id vertex-id) *neighbours*)
            (list (list 'arc graph-id vertex-id vertexx-id w)))
    (push (list 'arc graph-id vertex-id vertexx-id w)
          (gethash (list graph-id vertex-id) *neighbours*)))
  (if (not (gethash (list graph-id vertexx-id) *neighbours*))
      (setf (gethash (list graph-id vertexx-id) *neighbours*)
            (list (list 'arc graph-id vertexx-id vertex-id w)))
    (push (list 'arc graph-id vertexx-id vertex-id w)
          (gethash (list graph-id vertexx-id) *neighbours*)))
  (if (gethash (list 'arc graph-id vertexx-id vertex-id) *arcs*)
      (remhash (list 'arc graph-id vertexx-id vertex-id) *arcs*))
  (or (equal vertex-id vertexx-id)
      (setf (gethash (list 'arc graph-id vertex-id vertexx-id) *arcs*)
            (list 'arc graph-id vertex-id vertexx-id w))))



(defun graph-arcs (graph-id)
  (if (gethash graph-id *graphs*)
      (let((l '()))
	(maphash #'(lambda (k v)
		     (when(equal (second k) graph-id)
		       (push v l)))
		 *arcs*)
	l)
    nil))



(defun graph-vertex-neighbours (graph-id vertex-id)
  (nth-value 0 (gethash (list graph-id vertex-id) *neighbours*)))



(defun graph-vertex-adjacent (graph-id vertex-id)
  (if (and (gethash graph-id *graphs*)
           (atom vertex-id)
           (gethash (list 'vertex graph-id vertex-id) *vertices*))
      (let(( l ' ()))
        (maphash #'(lambda (k v)
		     (cond
                      ((and
			(equal (third k) vertex-id)
                        (equal (second k) graph-id))
                       (push (list 'vertex graph-id (fourth k))
			     l))
                      ((and
			(equal (fourth k) vertex-id)
                        (equal (second k) graph-id))
                       (push (list 'vertex graph-id (third k))
			     l))))
		 *arcs*)
	l)
    nil ))



(defun graph-print (graph-id)
  (if (gethash graph-id *graphs*)
      (let((l ' ()))
        (maphash #'(lambda (k v)
		     (when (equal (second k) graph-id)
		       (push v l)))
		 *arcs*)
        (maphash #'(lambda (k v)
		     (when (equal (second k) graph-id)
		       (push v l)))
		 *vertices*)
	l)
    nil))

;-------------------------------------------------------------------

(defstruct node
  key
  valore)



(defun new-heap (heap-id &optional(capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0
		  (make-array capacity
			      :fill-pointer 0
			      :adjustable t
			      :element-type '(or null node))))))



(defun heap-id (heap-rep)
  (second heap-rep))



(defun heap-size (heap-rep)
  (third heap-rep))




(defun heap-actual-heap (heap-rep)
  (fourth heap-rep))



(defun heap-delete (heap-id)
  (if (remhash heap-id *heaps*)
      (not (maphash #'(lambda (k v)
			(when (equal (first k) heap-id)
			  (remhash k *index*)))
		    *index*))
    t))



(defun heap-empty (heap-id)
  (if (gethash heap-id *heaps*)
      (cond
       ((equal (heap-size(gethash heap-id *heaps*)) 0) t)
       (t nil))
    nil))



(defun heap-not-empty (heap-id)
  (if (gethash heap-id *heaps*)
      (cond
       ((equal (heap-size (gethash heap-id *heaps*)) 0) nil)
       (t t))
    nil))



(defun heap-head (heap-id)
  (if (and (gethash heap-id *heaps*)
	   (> (length (heap-actual-heap (gethash heap-id *heaps*))) 0))
      (list (node-key (aref (heap-actual-heap (gethash heap-id *heaps*)) 0))
            (node-valore
	     (aref (heap-actual-heap (gethash heap-id *heaps*)) 0)))
    nil))



(defun heap-insert (heap-id k v)
  (if (and (gethash heap-id *heaps*)
	   (not (gethash (list heap-id v) *index*)))
      (let((l (gethash heap-id *heaps*))
           (p (truncate (/ (float (- (heap-size (gethash heap-id *heaps*))1))
			   2)))
           (i t))
	(vector-push-extend
	 (make-node :key k :valore v)
	 (heap-actual-heap l)
	 1)
	(setf (gethash (list heap-id v) *index*) (heap-size l))
	(setf (third l) (+ 1 (third l)))
	(heapify-reverse heap-id (heap-actual-heap l) p p) l p i)
    nil))



(defun heap-exchange (heap-id array p1 p2)
  (if(/= p1 p2)
      (let((l (aref array p1))
           (r (aref array p2)))
	(setf (aref array p1) r)
	(setf (aref array p2) l)
	(setf (gethash (list heap-id (node-valore l)) *index*)
	      p2)
	(setf (gethash (list heap-id (node-valore r)) *index*)
	      p1)
	l r)
    t))



(defun stampalista (heap-id nodo l)
  (heapify heap-id (heap-actual-heap l) 0 0)
  (setf (gethash (list heap-id (node-valore nodo)) *index*)
	nil)
  (list (node-key nodo) (node-valore nodo)))



(defun heap-extract (heap-id)
  (if (and
       (gethash heap-id *heaps*)
       (> (length (heap-actual-heap (gethash heap-id *heaps*))) 0))
      (and
       (let(( l (gethash heap-id *heaps*)))
         (heap-exchange heap-id (heap-actual-heap l) 0 (- (heap-size l) 1))
         (setf (third l) (- (heap-size l) 1))
	 l)
       (stampalista heap-id
		    (vector-pop (heap-actual-heap (gethash heap-id *heaps*)))
                    (gethash heap-id *heaps*))
       )
    nil))



(defun heap-modify-key (heap-id new-key old-key v)
  (if (and
       (gethash heap-id *heaps*)
       (gethash (list heap-id v) *index*)
       (equal (node-key (aref (heap-actual-heap (gethash heap-id *heaps*))
                              (gethash (list heap-id v) *index*)))
              old-key))
      (let (( l (heap-actual-heap (gethash heap-id *heaps*)))
            (p (gethash (list heap-id v) *index*))
            (i t))
        (let ((i (truncate (/ (float (- p 1)) 2))))
          (setf (node-key (aref l  p))
		new-key)
          (heapify-reverse heap-id l i i)
          (heapify heap-id l p p)
	  i)
	l p i)
    nil))



(defun modify-key (heap-id new-key v)
  (if (and
       (gethash heap-id *heaps*)
       (gethash (list heap-id v) *index*))
      (let (( l (heap-actual-heap (gethash heap-id *heaps*)))
            (p (gethash (list heap-id v) *index*))
            (i t))
	(let ((i (truncate (/ (float (- p 1)) 2))))
          (setf (node-key (aref l  p))
            	new-key)
          (heapify-reverse heap-id l i i)
          (heapify heap-id l p p)
     	  i)
     	l p i)
    nil))



(defun heap-print (heap-id)
  (heap-actual-heap (gethash heap-id *heaps*)))



(defun rkey (nodo1 nodo2)
  (< (node-key  nodo1)
     (node-key nodo2)))



(defun heapify (heap-id array i min)
  (let ((l (+ 1 (* 2 i)))
	(r (+ 2 (* 2 i))))
    (cond
     ((and (/= min l)
           (< l (length array))
           (rkey (aref array l) (aref array min)))
      (heapify heap-id array i l))

     ((and (/= min r)
           (< r (length array))
           (rkey (aref array r) (aref array min)))
      (heapify heap-id array i r))

     ((/= min i)
      (and (heap-exchange heap-id array i min)
           (heapify heap-id array min min)))
     ((= i min) t))
    l r))



(defun heapify-reverse (heap-id array i min)
  (let ((l (+ 1 (* 2 i)))
	(r (+ 2 (* 2 i)))
	(p(truncate (/ (float (- i 1)) 2))))
    (cond
     ((and (/= min l)
           (< l (length array))
           (rkey (aref array l) (aref array min)))
      (heapify-reverse heap-id array i l))

     ((and (/= min r)
           (< r (length array))
           (rkey (aref array r) (aref array min)))
      (heapify-reverse heap-id array i r))
     ((/= min i)
      (and (heap-exchange heap-id array i min)
           (heapify-reverse heap-id array p p)))
     ((= i min) t))
    l r p))



(defun mst-vertex-key (graph-id vertex-id)
  (let
      ((g (gethash (list graph-id vertex-id) *vertex-keys*)))
    g))



(defun mst-previous (graph-id vertex-id)
  (let
      ((g (gethash (list graph-id vertex-id) *previous*)))
    g))



(defun mst-prim (graph-id source)
  (initial-prim graph-id source))



(defun initial-prim (graph-id source)
  (not (n-node graph-id))
  (heap-delete 'h)
  (new-heap 'h (hash-table-count *vertices*))
  (heap-insert 'h 0 source)
  (prim-ex graph-id))



(defun prim-ex (graph-id)
  (cond((and
         (heap-not-empty 'h)
         (let(( l (heap-extract 'h)))
           (setf (gethash (list graph-id (second l)) *vertex-keys*)
		 (first l))
           (update-key
	    graph-id
	    (second l)
	    (graph-vertex-neighbours graph-id (second l)))
           (prim-ex graph-id) l))
	nil)
       ((heap-empty 'h) (not (heap-delete 'h)))))



(defun n-node (graph-id)
  (maphash #'(lambda (k v)
	       (when (equal (second k) graph-id)
                 (and
                  (setf (gethash (list graph-id (third k)) *previous*)
			-1)
                  (setf (gethash (list graph-id (third k)) *vertex-keys*)
			most-positive-double-float))))
           *vertices*))



(defun update-key (graph-id source neighbours)
  (if (> (length neighbours) 0)
      (and
       (let((arco (car neighbours)))
         (let ((indice (gethash (list 'h (fourth arco)) *index*))
               (vertex (fourth arco))
               (w (fifth arco)))
           (cond ((equal nil indice)
                  (if (equal most-positive-double-float
                             (gethash (list graph-id vertex) *vertex-keys*))
                      (and
		       (heap-insert 'h w vertex)
                       (setf (gethash (list graph-id vertex) *previous*)
			     source))
                    t))
                 (t (if (< w
			   (node-key(aref (fourth (gethash 'h *heaps*))
					  indice)))
                        (and
			 (modify-key 'h w vertex)
                         (setf (gethash (list graph-id vertex) *previous*)
			       source))
                      t)))
	   indice vertex w)
	 arco)
       (update-key graph-id source (cdr neighbours)))
    t))



(defun mst-get (graph-id source)
  (clrhash *adjacent*)
  (clrhash *nodes*)
  (not(begin graph-id))
  (during graph-id source '()))



(defun begin (graph-id)
  (maphash #'(lambda (k v)
	       (when (and
		      (equal graph-id (first k))
                      (not(equal -1 v)))
                 (and
		  (setf
		   (gethash k *nodes*)
		   (list v (gethash k *vertex-keys*)))
                  (if (not (gethash (list graph-id v) *adjacent*))
                      (setf (gethash (list graph-id v) *adjacent*)
                            (list
			     (list
			      (second k)
			      (gethash k *vertex-keys*))))
                    (push (list (second k) (gethash k *vertex-keys*))
                          (gethash (list graph-id v) *adjacent*))))))
	   *previous*))



(defun during (graph-id source padri)
  (cond ((> (length (gethash (list graph-id source) *adjacent*)) 1)
         (during-print 0
                       graph-id
                       source
                       (sorting
			(remove-ad
			 (gethash (list graph-id source) *adjacent*)
			 (min-key
			  (gethash (list graph-id source)
				   *adjacent*))))
                       nil
                       padri
                       (gethash (list graph-id source)
				*adjacent*)))

        ((equal (length (gethash (list graph-id source)
				 *adjacent*))
		1)
         (during-print 1
                       graph-id
                       source
                       (first (car
			       (gethash (list graph-id source) *adjacent*)))
                       (second (car
				(gethash (list graph-id source) *adjacent*)))
                       padri
                       (gethash (list graph-id source) *adjacent*)))

        ((and
	  (equal
	   (gethash (list graph-id source) *adjacent*)
	   nil)
          (>
	   (hash-table-count *nodes*)
	   0))
         (during graph-id (car padri) (cdr padri)))
        (t nil)))



(defun during-print (id graph-id source v k padri adiacenti)
  (if(equal id 0)
      (progn
	(remhash (list graph-id v) *nodes*)
	(setf
	 (gethash (list graph-id source) *adjacent*)
	 (remove-add adiacenti v))
	(cons
	 (list 'arc
	       graph-id
	       source
	       v
	       (gethash (list graph-id v) *vertex-keys*))
	 (during graph-id v (push source padri))))
    (progn
      (remhash (list graph-id v) *nodes*)
      (remhash (list graph-id source) *adjacent*)
      (cons
       (list 'arc graph-id source v k)
       (during graph-id v padri)))))



(defun r-other (adiacenti k)
  (cond
   ((null
     (car adiacenti))
    nil)
   ((equal
     (second (car adiacenti))
     k)
    (cons
     (write-to-string (first (car adiacenti)))
     (r-other (cdr adiacenti) k)))
   (t (r-other (cdr adiacenti) k))))



(defun min-key (adiacenti)
  (get-min (second (car adiacenti))
           (cdr adiacenti)))



(defun get-min (min l)
  (cond
   ((null (car l)) min)
   ((< (second (car l)) min)
    (get-min (second (car l)) (cdr l)))
   (t (get-min min (cdr l)))))



(defun remove-ad (adiacenti k)
  (cond
   ((null (car adiacenti)) nil)
   ((equal (second (car adiacenti)) k)
    (cons (first (car adiacenti))
	  (remove-ad (cdr adiacenti) k)))
   (t (remove-ad (cdr adiacenti) k))))



(defun remove-add (adiacenti k)
  (cond
   ((equal (first (car adiacenti)) k)
    (cdr adiacenti))
   (t (cons (car adiacenti)
	    (remove-add (cdr adiacenti) k)))))


(defun mst-print (graph-id)
  (maphash #'(lambda (k v)
               (when (equal (first k) graph-id)
                 (print (list 'vertex-key (second k) v))))
           *vertex-keys*)
  (maphash #'(lambda (k v)
               (when (equal (first k) graph-id)
                 (print (list 'previous (second k) v))))
           *previous*))



(defun sorting (lista)
  (alpha-first(car lista) (cdr lista)))



(defun alpha-first (m lista)
  (cond
   ((null lista) m)
   ((minore (car lista) m)
    (alpha-first (car lista) (cdr lista)))
   (t (alpha-first m (cdr lista)))))



(defun minore (a b)
  (cond
   ((and (numberp a)
	 (numberp b))
    (< a b))
   ((and (not (numberp a))
	 (numberp b))
    nil)
   ((and (numberp a)
	 (not (numberp b)))
    t)
   (t (not (null (string< (write-to-string a)
			  (write-to-string b)))))))
