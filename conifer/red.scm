(define-module (conifer red)
	       #:export (make-view
			  [red-node-offset . offset]
			  [red-node-parent . parent]
			  red-node-green
			  children
			  red-node?))

(use-modules (rnrs records syntactic)
	     (conifer green))

(define-record-type
  red-node
  (fields
    ; Absolute offset into the file.
    offset
    ; Pointer to the (red node) parent (#f for root).
    parent
    ; The green node into which this red node peeks.
    green))

(define make-view
  (lambda (green-tree)
    (make-red-node
      0
      #f
      green-tree)))

(define children
  (lambda (parent)
    (when (token? (red-node-green parent))
      (assertion-violation 'children
			   "cannot get children of a token"))
    (let loop ([offset (red-node-offset parent)]
	       [children (node-children (red-node-green parent))]
	       [acc '()])
      (if (null? children)
	(reverse acc)
	(let ([first (car children)]
	      [rest (cdr children)])
	  (loop (+ offset (tree-text-length first))
		rest
		(cons (make-red-node offset
				     parent
				     first)
		      acc)))))))

