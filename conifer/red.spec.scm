;(use-modules (srfi srfi-64)
;	     (conifer green)
;	     (conifer red)
;	     (rnrs base))
;
;(define
;  green (let ([b (green-node-builder)])
;	  (start-node b 'root)
;
;	  (start-node b 'bin-expr)
;
;	  (start-node b 'literal)
;	  (push-token b 'int-number "5")
;	  (finish-node b)
;
;	  (push-token b 'whitespace " ")
;	  (push-token b 'plus "+")
;	  (push-token b 'whitespace " ")
;
;	  (start-node b 'literal)
;	  (push-token b 'int-number "10")
;	  (finish-node b)
;
;	  (finish-node b)
;	  (finish-node b)
;	  (finish-builder b)))
;
;(define red (make-view green))
;
;(display red)
;(newline)
;(display (offset red))
;(newline)
;(newline)
;(for-each (lambda (c)
;	    (for-each (lambda (c)
;			(display "offset: ")
;			(display (offset c))
;			(newline)
;			(display "parent: ")
;			(display (parent c))
;			(newline)
;			(display "green: ")
;			(display (red-node-green c))
;			(newline))
;		      (children c)))
;	  (children red))
;(newline)
