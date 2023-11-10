(library
  (conifer green)
  (export start-node
	  finish-node
	  push-token
	  finish-builder
	  tree->string
	  tree->debug-string
	  syntax-kind
	  text-length
	  node-children
	  token-text
	  node?
	  token?
	  (rename (make-builder green-node-builder)))
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs mutable-pairs)
		set-cdr!)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs io ports)
		open-string-output-port)
	  (only (rnrs io simple)
		display
		newline)
	  (only (rnrs hashtables)
		make-eqv-hashtable
		equal-hash
		hashtable-ref
		hashtable-set!)
	  (only (rnrs lists)
		for-all
		cons*
		find))

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
      (mutable node-stack)
      ; Hash table of interned nodes. HashTable[Hash, List[Node]]
      nodes
      ; Hash table of interned tokens. HashTable[Hash, List[Token]]
      tokens)
    (protocol
      (lambda (new)
	(lambda ()
	  ; current-node
	  (let ([root (list 'builder)])
	    (new root
		 ; node-stack
		 '()
		 (make-eqv-hashtable)
		 (make-eqv-hashtable)))))))

  (define-record-type
    node
    (fields
      ;; Kind of the syntax represented by the node. **Must** be eq?-able. (e.g. 'list, 'root, etc.)
      syntax-kind
      ;; Cached length of all the tokens under the tree.
      text-length
      ;; Cached hash of the current node to "intern" them.
      hash
      ;; Vector of all the node and token children.
      children))

  (define-record-type
    token
    (fields
      ;; Kind of the syntax represented by the token. **Must** be eq?-able. (e.g. 'int-number, 'string, etc.)
      syntax-kind
      ;; Text that comprises the token.
      text))

  ;; Returns the syntax kind of the node/token.
  (define syntax-kind
    (lambda (t)
      ((if (node? t)
	 node-syntax-kind
	 token-syntax-kind)
       t)))

  ;; Returns the text length of the token.
  (define text-length
    (lambda (t)
      (if (node? t)
	(node-text-length t)
	(string-length (token-text t)))))

  ;; Returns the hash of the node/text.
  ;;
  ;; For tokens, the hash of the 
  (define tree-hash
    (lambda (t)
      (if (node? t)
	(node-hash t)
	(equal-hash t))))

  ;; Calculates the hash of the children list.
  (define children-hash
    (lambda (t)
      (equal-hash (map tree-hash t))))

  ;; Calculates the text-length of a list of children.
  (define children-text-length
    (lambda (children)
      (apply + (map text-length children))))

  ;; Compares two nodes. Since all nodes and tokens should be unique (two 
  (define node=?
    (lambda (n1 n2)
      (and (node? n1)
	   (node? n2)
	   (eq? (node-syntax-kind n1)
		(node-syntax-kind n2))
	   (= (node-text-length n1)
	      (node-text-length n2))
	   (= (node-hash n1)
	      (node-hash n2))
	   (= (vector-length (node-children n1))
	      (vector-length (node-children n2)))
	   (for-all eq?
		    (vector->list (node-children n1))
		    (vector->list (node-children n2))))))

  ;; Given a syntax-kind and text, returns the interned token if already present
  ;; or interns a new one and returns it.
  (define get-or-intern-token
    (lambda (b syntax-kind text)
      (let ([hash (equal-hash (cons syntax-kind text))]
	    [hashtable (builder-tokens b)])
	(cond
	  [(hashtable-ref hashtable
			  hash
			  #f) =>
	   (lambda (tok)
	     ; Make sure the interned token is the same as the one we want.
	     (when (not (and (eq? (token-syntax-kind tok) syntax-kind)
			     (string=? (token-text tok) text)))
	       (assertion-violation
		 'get-or-intern-token
		 "found a hash collision, between ('token ~a . ~a) and ('token ~a . ~a)"
		 (token-syntax-kind tok)
		 (token-text tok)
		 syntax-kind
		 text))
	     tok)]
	  [else (let ([token (make-token syntax-kind text)])
		  (hashtable-set! hashtable
				  hash
				  token)
		  token)]))))

  ;; Given a syntax-kind and list of children, returns an already interned node,
  ;; or creates a new one, interns it and returns it.
  (define get-or-intern-node
    (lambda (b syntax-kind children)
      (let* ([hashtable (builder-nodes b)]
	     [tlen (children-text-length children)]
	     [hash (equal-hash (cons* syntax-kind tlen (children-hash children)))])
	(let ([nodes (hashtable-ref hashtable hash '())])
	  (cond
	    [(find (lambda (n) (and (eq? (node-syntax-kind n)
					 syntax-kind)
				    (= (node-text-length n)
				       tlen)
				    (for-all eq?
					     (vector->list (node-children n))
					     children)))
		   nodes)
	     => (lambda (node) node)]
	    [else (let ([node (make-node syntax-kind
					 tlen
					 hash
					 (list->vector children))])
		    (hashtable-set! hashtable
				    hash
				    (cons node nodes))
		    node)])))))

  ;; Starts a new active node with `syntax-kind` and suspends the old active node.
  (define start-node
    (lambda (b syntax-kind)
      ; Push the current node to the stack of suspended parents.
      (builder-node-stack-set!
	b
	(cons (builder-current-node b)
	      (builder-node-stack b)))

      ; Replace the active node with a new one
      (builder-current-node-set!
	b
	(cons syntax-kind '()))))

  ;; Pushes the token '(kind . text) as a child to the active node.
  (define push-token
    (lambda (b kind text)
      (let ([current-node (builder-current-node b)])
	(set-cdr! current-node
		  (cons (get-or-intern-token b kind text)
			(cdr current-node))))))

  ;; Finishes the active node, pushing it as a child of the suspended node and turning the suspended
  ;; node into the active one.
  (define finish-node
    (lambda (b)
      (let ([builder-node (builder-current-node b)]
	    [parent-node (car (builder-node-stack b))])
	; Pop the parent node
	(builder-node-stack-set! b (cdr (builder-node-stack b)))

	(let ([tlen (children-text-length (cdr builder-node))]
	      [hash (children-hash (cdr builder-node))])
	  (set-cdr! parent-node
		    (cons (get-or-intern-node
			    b
			    ; syntax-kind
			    (car builder-node)
			    ; children
			    (reverse (cdr builder-node)))
			  (cdr parent-node))))
	; Set the current node's text-len
	(builder-current-node-set!
	  b
	  parent-node))))

  ;; Pushes the token '(kind . text) to the current node.
  (define finish-builder
    (lambda (b)
      (let ([current-node (builder-current-node b)])
	(if (and (= (length (cdr current-node))
		    1)
		 (eq? (car current-node)
		      'builder))
	  (cadr (builder-current-node b))
	  (assertion-violation
	    'finish-builder
	    "Not all nodes have been finished")))))

  ;; Returns the text represented by `t`, by appending the text of all the tokens.
  (define tree->string
    (lambda (t)
      (let-values ([(out g) (open-string-output-port)])
	(let inner-tree->string ([t t])
	  (if (node? t)
	    (vector-for-each inner-tree->string (node-children t))
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
	    (display (+ offset (text-length t)) out)
	    (cond
	      [(node? t)
	       (newline out)
	       (let ([children (node-children t)]
		     [length (vector-length (node-children t))])
		 (let loop ([offset offset]
			    [children-i 0])
		   (when (< children-i length)
		     (inner-tree->debug-string (vector-ref children children-i)
					       offset
					       (+ indentation-width INDENTATION_WIDTH))
		     (loop (+ offset (text-length (vector-ref children children-i)))
			   (+ children-i 1)))))]
	      [else
		(display " \"" out)
		(display (token-text t) out)
		(display "\"" out)
		(newline out)])))
	(g)))))
