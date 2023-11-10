(import (srfi srfi-64)
	(conifer green))

(test-runner-current (test-runner-simple))

(test-group
  "basic tree"
  (define t
    (let ([b (green-node-builder)])
      (start-node b 'root)

      (start-node b 'bin-expr)

      (start-node b 'literal)
      (push-token b 'int-number "5")
      (finish-node b)

      (push-token b 'whitespace " ")
      (push-token b 'plus "+")
      (push-token b 'whitespace " ")

      (start-node b 'literal)
      (push-token b 'int-number "10")
      (finish-node b)

      (finish-node b)

      (finish-node b)
      (finish-builder b)))

  (test-equal "5 + 10" (tree->string t))
  (test-equal "\
root@0..6
  bin-expr@0..6
    literal@0..1
      int-number@0..1 \"5\"
    whitespace@1..2 \" \"
    plus@2..3 \"+\"
    whitespace@3..4 \" \"
    literal@4..6
      int-number@4..6 \"10\"
" (tree->debug-string t)))

(test-group
  "interning"
  (define t
    (let ([b (green-node-builder)])
      (define push-1
	(lambda ()
	  (start-node b 'literal)
	  (push-token b 'int-number "1")
	  (finish-node b)))
      (define push-bin-expr
	(lambda ()
	  (start-node b 'bin-expr)
	  (push-token b 'delim "(")
	  (push-1)
	  (push-token b 'whitespace " ")
	  (push-token b 'plus "+")
	  (push-token b 'whitespace " ")
	  (push-1)
	  (push-token b 'delim ")")
	  (finish-node b)))

      (start-node b 'root)
      (start-node b 'bin-expr)
      (push-bin-expr)
      (push-token b 'whitespace " ")
      (push-token b 'star "*")
      (push-token b 'whitespace " ")
      (push-bin-expr)
      (finish-node b)
      (finish-node b)
      (finish-builder b)))
  (test-equal "(1 + 1) * (1 + 1)" (tree->string t))

  ; All 1s must be a single token.
  ; Same with whitespaces.
  ; Same with +.
  ; Same with ( and ).
  ; Both (1 + 1) must be a single node.
  (let ([root-children (node-children t)])
    (test-eqv 1 (vector-length root-children))
    (let ([root-bin-expr-children (node-children (vector-ref root-children 0))])
      (test-eqv 5 (vector-length root-bin-expr-children))
      (let ([bin-expr-1 (vector-ref root-bin-expr-children
				    0)]
	    [bin-expr-2 (vector-ref root-bin-expr-children
				    4)]
	    [whitespace-token (vector-ref root-bin-expr-children
					  1)])
	(test-eq bin-expr-1
		 bin-expr-2)
	(test-eq whitespace-token
		 (vector-ref root-bin-expr-children
			     3))
	(let ([be1-children (node-children bin-expr-1)]
	      [be2-children (node-children bin-expr-2)])
	  (for-each
	    (lambda (l r)
	      (test-eq l r))
	    (vector->list be1-children)
	    (vector->list be2-children))

	  ; Test all whitespace are the same
	  (test-eq (vector-ref be1-children 2)
		   whitespace-token)
	  (test-eq (vector-ref be1-children 4)
		   whitespace-token)

	  ; Test all 1s are the same
	  (test-eq (vector-ref be1-children 1)
		   (vector-ref be1-children 5))
	  (test-eq (vector-ref be2-children 1)
		   (vector-ref be2-children 5)))))))
