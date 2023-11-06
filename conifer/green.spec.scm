(use-modules (srfi srfi-64)
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
