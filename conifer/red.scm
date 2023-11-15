(library
  (conifer red)
  (export make-view
	  red-tree-green
	  children
	  red-tree?
	  (rename (red-tree-offset offset)
		  (red-tree-parent parent)))
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (conifer green)
		node-children
		node?
		token?
		text-length))

  ;; A red tree is a view into the green tree, and through which users should
  ;; interact with the green tree. They are meant to be disposable and cheap
  ;; to make, since only the green trees are meant to be kept around.
  ;;
  ;; A red tree keeps a bit of bookkeeping. It knows what is its absolute
  ;; offset into the text and keeps a pointer to its parent.
  (define-record-type
    red-tree
    (fields
      ; Absolute offset into the file.
      offset
      ; The green tree into which this red tree peeks.
      green
      ; Pointer to the (red tree) parent (#f for root).
      parent))

  ;; Creates a red tree from a green tree.
  (define make-view
    (lambda (green-tree)
      (when (not (or (node? green-tree)
		     (token? green-tree)))
	(assertion-violation
	  'make-view
	  "expected a greent tree ~A"
	  green-tree))
      (make-red-tree
	0
	green-tree
	#f)))

  ;; Returns a list of red trees for all the children of the current red tree
  (define children
    (lambda (parent)
      (when (token? (red-tree-green parent))
	(assertion-violation 'children
			     "cannot get children of a token"))
      (let* ([children (node-children (red-tree-green parent))]
	     [len (vector-length children)])
	(let loop ([offset (red-tree-offset parent)]
		   [n 0]
		   [acc '()])
	  (if (= n len)
	    (list->vector (reverse acc))
	    (let ([child (vector-ref children n)])
	      (loop (+ offset (text-length child))
		    (+ n 1)
		    (cons (make-red-tree offset
					 child
					 parent)
			  acc)))))))))
