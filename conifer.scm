(define-module (conifer))

(use-modules (conifer green)
	     (conifer red))

(export conifer-text-length
	conifer-tree->string
	conifer-syntax-kind
	conifer-tree->debug-string
	conifer-children
	conifer-token-text
	conifer-node?
	conifer-token?
	conifer-green-tree?)

(re-export
  ; Green node builder functions
  (green-node-builder . conifer-green-node-builder)
  (start-node . conifer-start-node)
  (finish-node . conifer-finish-node)
  (push-token . conifer-push-token)
  (finish-builder . conifer-finish-builder)

  ; Red tree builder functions
  (make-view . conifer-make-view)

  ; Green tree specific functions
  (text-length . conifer-green-text-length)
  (syntax-kind . conifer-green-syntax-kind)
  (tree->string . conifer-green-tree->string)
  (tree->debug-string . conifer-green-tree->debug-string)
  (node-children . conifer-green-node-children)
  (token-text . conifer-green-token-text)
  (node? . conifer-green-node?)
  (token? . conifer-green-token?)

  ; Red tree specific functions
  (children . conifer-red-children)
  (offset . conifer-red-offset)
  (parent . conifer-red-parent)
  (red-node-green . conifer-red-node-green)
  (red-node? . conifer-red-node?))

;; Creates a generic function that works on both red and green trees, by either directly
;; calling the specific function on the green node, or dereferencing the green node first,
;; and then doing the call.
(define-syntax generic-red-green
  (syntax-rules ()
    [(_ generic-fn-name green-fn-name)
     (define generic-fn-name
       (lambda (t)
	 (green-fn-name (if (red-node? t)
			  (red-node-green t)
			  t))))]))

(generic-red-green conifer-text-length text-length)
(generic-red-green conifer-syntax-kind syntax-kind)
(generic-red-green conifer-tree->string tree->string)
(generic-red-green conifer-tree->string tree->string)
(generic-red-green conifer-tree->debug-string tree->debug-string)
(generic-red-green conifer-token-text token-text)
(generic-red-green conifer-node? node?)
(generic-red-green conifer-token? token?)

(define conifer-children
  (lambda (t)
    ((if (red-node? t)
       children
       node-children)
     t)))

(define conifer-green-tree?
  (lambda (t)
    (or (node? t)
	(token? t))))
