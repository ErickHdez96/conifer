(import (srfi srfi-64)
	(conifer))

(test-begin "conifer")

(define b (conifer-green-node-builder))

(define
  root
  (begin
    (conifer-start-node b 'root)
    (begin
      (conifer-start-node b 'binop)
      (begin
	(conifer-push-token b 'int "5")
	(conifer-push-token b 'plus "+")
	(conifer-push-token b 'int "10"))
      (conifer-finish-node b))
    (conifer-finish-node b)
    (conifer-finish-builder b)))

; Exact same as `root`
(define
  root2
  (begin
    (conifer-start-node b 'root)
    (conifer-push-node b (vector-ref (conifer-green-node-children root) 0))
    (conifer-finish-node b)
    (conifer-finish-builder b)))

; binop node same as `root`
(define root3
  (begin
    (conifer-start-node b 'root3)
    (begin
      (conifer-start-node b 'binop)
      (begin
	(conifer-push-token b 'int "5")
	(conifer-push-token b 'plus "+")
	(conifer-push-token b 'int "10"))
      (conifer-finish-node b))
    (conifer-finish-node b)
    (conifer-finish-builder b)))

; only numbers same as `root`
(define root4
  (begin
    (conifer-start-node b 'root4)
    (begin
      (conifer-start-node b 'binop)
      (begin
	(conifer-push-token b 'int "5")
	(conifer-push-token b 'minus "-")
	(conifer-push-token b 'int "10"))
      (conifer-finish-node b))
    (conifer-finish-node b)
    (conifer-finish-builder b)))

; root2 and root are exactly the same and built from the same builder.
; both must have been interned and therefore be the same object
(test-eq root2 root)
(test-assert (not (eq? root3 root)))
(test-assert (not (eq? root4 root)))

(let ([red (conifer-make-view root)]
      [red2 (conifer-make-view root2)]
      [red3 (conifer-make-view root3)]
      [red4 (conifer-make-view root4)])
  (test-equal
    "5+10"
    (conifer-tree->string red))
  (test-equal
    "5+10"
    (conifer-tree->string red2))
  (test-equal
    "5+10"
    (conifer-tree->string red3))
  (test-equal
    "5-10"
    (conifer-tree->string red4))

  (let ([red-bin-op (vector-ref (conifer-red-children red) 0)]
	[red3-bin-op (vector-ref (conifer-red-children red3) 0)]
	[red4-bin-op (vector-ref (conifer-red-children red4) 0)])
    ; root and root3 share the same child
    (test-eq (conifer-red-tree-green red-bin-op)
	     (conifer-red-tree-green red3-bin-op))
    (test-assert
      (not (eq? (conifer-red-tree-green red-bin-op)
		(conifer-red-tree-green red4-bin-op))))

    (let ([red-children (conifer-red-children red-bin-op)]
	  [red4-children (conifer-red-children red4-bin-op)])
      ; Num5 is the same
      (test-eq (conifer-red-tree-green (vector-ref red-children 0))
	       (conifer-red-tree-green (vector-ref red4-children 0)))
      ; The operators are unique
      (test-assert
	(not (eq? (conifer-red-tree-green (vector-ref red-children 1))
		  (conifer-red-tree-green (vector-ref red4-children 1)))))
      ; Num10 are the same
      (test-eq (conifer-red-tree-green (vector-ref red-children 2))
	       (conifer-red-tree-green (vector-ref red4-children 2))))))

(test-end "conifer")
