(import (rnrs base)
	(srfi srfi-64)
	(conifer green)
	(conifer red))

(test-runner-current (test-runner-simple))

(test-group
  "simple red-tree view"
  (define red-tree
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
      (make-view (finish-builder b))))
  (define red-children (children red-tree))

  (test-eqv
    1
    ; root
    ; -- bin-expr
    (length red-children))

  (let ([bin-expr-node (car red-children)])
    (test-eq
      'bin-expr
      (syntax-kind (red-tree-green bin-expr-node)))
    (test-eqv
      0
      (offset bin-expr-node))
    (test-eq
      red-tree
      (parent bin-expr-node))

    (let ([bin-expr-children (children bin-expr-node)])
      (test-eqv
	5
	; bin-expr
	; -- literal
	; -- whitespace
	; -- plus
	; -- whitespace
	; -- literal
	(length bin-expr-children))
      (let ([literal-five (car bin-expr-children)])
	(test-eq
	  'literal
	  (syntax-kind (red-tree-green literal-five)))
	(test-eqv
	  0
	  (offset literal-five))
	(let ([literal-five-children (children literal-five)])
	  (test-eqv
	    1
	    ; literal
	    ; -- int-number
	    (length literal-five-children))
	  (test-eq
	    'int-number
	    (syntax-kind (red-tree-green (car literal-five-children))))
	  (test-eqv
	    0
	    (offset (car literal-five-children)))
	  (test-equal
	    "5"
	    (token-text (red-tree-green (car literal-five-children))))))

      (let ([whitespace (cadr bin-expr-children)])
	(test-eq
	  'whitespace
	  (syntax-kind (red-tree-green whitespace)))
	(test-eqv
	  1
	  (offset whitespace))
	(test-equal
	  " "
	  (token-text (red-tree-green whitespace))))

      (let ([plus (caddr bin-expr-children)])
	(test-eq
	  'plus
	  (syntax-kind (red-tree-green plus)))
	(test-eqv
	  2
	  (offset plus))
	(test-equal
	  "+"
	  (token-text (red-tree-green plus))))

      (let ([whitespace-2 (cadddr bin-expr-children)])
	(test-eq
	  'whitespace
	  (syntax-kind (red-tree-green whitespace-2)))
	(test-eqv
	  3
	  (offset whitespace-2))
	(test-equal
	  " "
	  (token-text (red-tree-green whitespace-2))))

      (let ([literal-ten (car (cddddr bin-expr-children))])
	(test-eq
	  'literal
	  (syntax-kind (red-tree-green literal-ten)))
	(test-eqv
	  4
	  (offset literal-ten))
	(let ([literal-ten-children (children literal-ten)])
	  (test-eqv
	    1
	    ; literal
	    ; -- int-number
	    (length literal-ten-children))
	  (test-eq
	    'int-number
	    (syntax-kind (red-tree-green (car literal-ten-children))))
	  (test-eqv
	    4
	    (offset (car literal-ten-children)))
	  (test-equal
	    "10"
	    (token-text (red-tree-green (car literal-ten-children)))))))))
