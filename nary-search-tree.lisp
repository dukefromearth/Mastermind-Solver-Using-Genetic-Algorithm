;; counts lists
(defun list-counter (list)
	(cond
		((endp list) 0)
		((listp (first list)) (+ 1 (list-counter (rest list)))) 
		(T (+ 0 (list-counter(rest list))))))

;; Deletes tree
(defun nary-tree-make-empty ()
  nil)

;; True if leaf or empty
(defun leaf_p (tree)
   (cond
     ((endp tree))
     ((atom (first tree)) (leaf_p (rest tree)))
     (T nil)))

;; True if tree
(defun node_p (tree)
  (cond
    ((endp tree) nil)
    ((listp (first tree)) T)
    (T (node_p (rest tree)))))

;; True if empty
(defun empty_p (tree)
  (null tree))

(defun nary-tree-depth (tree)
  (if (atom tree) 0
      (+ 1 (nary-tree-depth (first (rest tree))))))

;; Remove elem from top level of tree
(defun nary-tree-remove (elem tree)
  (if (empty_p tree)
      tree
    (if (leaf_p tree)
	(nary-tree-remove-leaf elem tree)
	(nary-tree-remove-node elem tree))))

(defun nary-tree-remove-leaf (elem leaf-list)
  (break)
  (remove elem leaf-list))

(defun nary-tree-remove-node (elem node)
  (if (and (eq (first node) elem) (= 1 (list-length node)))
      nil
      (delete-if #'(lambda (x) (equal (first x) elem)) node)))

(defun get-first-atom (ls)
  (if (listp ls)
      (if (atom (first ls)) (first ls)
	  (get-first-atom (first ls)))))

(defun nary-tree-get-head (tree)
  (labels
      ((aux (tree acc)
	 (if (listp (first tree)) (setf tree (first tree)))
	 (cond
	   ((endp tree) (reverse acc))
	   ((leaf_p tree) (reverse (cons (first tree) acc)))
	   (T (aux (rest tree) (cons (first tree) acc))))))
    (aux tree '())))
	 
