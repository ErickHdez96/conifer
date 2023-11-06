(define-module (conifer green)
	       #:export ([make-builder . green-node-builder]
			 start-node
			 finish-node
			 push-token
			 finish-builder
			 tree->string
			 tree->debug-string))

(use-modules (rnrs records syntactic)
	     (rnrs io ports)
	     (rnrs base))

;; A GreeTree builder.
;;
;; ## API
;;
;; * `start-node` starts a new green node and sets it as the active node.
;; * `finish-node` finishes the active node, pushes it as a child to the previously active node and returns to it.
;; * `push-token` registers a new token as a child of the currently active node.
;; * `finish-builder` finishes the builder and returns the built tree.
;;
;; The number of `start-node` and `finish-node` must match when finishing the builder. The tree must begin
;; with a root node, containing all other nodes.
;;
;; # Examples
;;
;; ```scheme
;; (display
;;   (tree->string
;;     (let ([b (make-builder)])
;;       (start-node b 'root)
;; 
;;       (start-node b 'binary-expr)
;; 
;;       (start-node b 'literal)
;;       (push-token b 'int-number "5")
;;       (finish-node b)
;; 
;;       (push-token b 'whitespace " ")
;;       (push-token b 'plus "+")
;;       (push-token b 'whitespace " ")
;; 
;;       (start-node b 'literal)
;;       (push-token b 'int-number "10")
;;       (finish-node b)
;; 
;;       (finish-node b)
;; 
;;       (finish-node b)
;;       (finish-builder b))))
;; (newline)
;; -> 5 + 10
;; ```
(define-record-type
  builder
  (fields
    ; Pair[syntax-kind, List[Node | Token]];
    (mutable current-node)
    ; List[Pair[syntax-kind, List[Node | Token]]]
    (mutable node-stack))
  (protocol
    (lambda (new)
      (lambda ()
	; current-node
	(let ([root '(builder)])
	(new root
	     ; node-stack
	     '()))))))

;; Starts a new active node with `syntax-kind` and suspends the old active node.
(define start-node
  (lambda (b syntax-kind)
    (define push-current-node!
      (lambda (b)
	(builder-node-stack-set!
	  b
	  (cons (builder-current-node b)
		(builder-node-stack b)))))

    (push-current-node! b)
    (builder-current-node-set!
      b
      (make-node syntax-kind))))

;; Pushes the token '(kind . text) as a child to the active node.
(define push-token
  (lambda (b kind text)
    (let ([current-node (builder-current-node b)])
      (set-cdr! current-node
		(cons (cons kind text)
		      (cdr current-node))))))

;; Finishes the active node, pushing it as a child the suspended node and turning the suspended
;; node into the active one.
(define finish-node
  (lambda (b)
    (let ([node (builder-current-node b)]
	  [parent-node (car (builder-node-stack b))])
      ; Pop the parent node
      (builder-node-stack-set! b (cdr (builder-node-stack b)))
      ; Push the current node into the parent's children.
      (set-cdr! parent-node
		(cons node (cdr parent-node)))
      ;; Adds the text-length to the finished node.
      (set-cdr! node
		(cons (children-text-length (cdr node))
		      (reverse (cdr node))))
      ; Set the current node's text-len
      (builder-current-node-set!
	b
	parent-node))))

;; Pushes the token '(kind . text) to the current node.
(define finish-builder
  (lambda (b)
    (if (eq? (car (builder-current-node b))
	     'builder)
      (cadr (builder-current-node b))
      (assertion-violation
	'finish-builder
	"Not all nodes have been finished"))))

(define is-node?
  (lambda (t)
    ; Nodes have three elements, while tokens only two. If the second element is a pair,
    ; then it must be a node.
    (pair? (cdr t))))

(define is-token?
  (lambda (t)
    (string? (cdr t))))

;; Returns the text length of the tree t.
(define tree-text-length
  (lambda (t)
    ((if (is-node? t)
       node-text-length
       token-text-length)
     t)))

;; Calculates the text-length of a list of children.
(define children-text-length
  (lambda (children)
    (apply + (map tree-text-length children))))

;; Creates a new node, with the given syntax-kind as its first element.
(define make-node list)
;; Returns the text length of the node
(define node-text-length cadr)

;; Returns the children of `node`.
(define node-children
  (lambda (n)
    (cddr n)))

;; Returns the text of the node.
(define token-text cdr)

;; Returns the text length of the node
(define token-text-length
  (lambda (t)
    (string-length (cdr t))))

;; Returns the syntax kind of the node or token.
(define syntax-kind car)

;; Returns the text represented by `t`, by appending the text of all the tokens.
(define tree->string
  (lambda (t)
    (let-values ([(out g) (open-string-output-port)])
      (let inner-tree->string ([t t])
	(if (is-node? t)
	  (for-each inner-tree->string (node-children t))
	  (display (token-text t) out)))
      (g))))

;; Returns a string debug representation for `t`, to see the nodes and tokens relationships.
(define tree->debug-string
  (lambda (t)
    (define INDENTATION_WIDTH 2)
    (let-values ([(out g) (open-string-output-port)])
      (let inner-tree->debug-string ([t t]
				     [offset 0]
				     [indentation-width 0])
	(let ([padding (make-string indentation-width #\space)])
	  (display padding out)
	  (display (syntax-kind t) out)
	  (display "@" out)
	  (display offset out)
	  (display ".." out)
	  (display (+ offset (tree-text-length t)) out)
	  (cond
	    [(is-node? t)
	     (newline out)
	     (let loop ([offset offset]
			[children (node-children t)])
	       (when (not (null? children))
		 (inner-tree->debug-string (car children)
					   offset
					   (+ indentation-width INDENTATION_WIDTH))
		 (loop (+ offset (tree-text-length (car children)))
		       (cdr children))))]
	    [else
	      (display " \"" out)
	      (display (token-text t) out)
	      (display "\"" out)
	      (newline out)])))
      (g))))
