
(load "include.lisp")

;------------------------------------------------
(defun encode-word (n word)
	(map 'list #'i2c 
		(map 'list #'(lambda (m) (mod (+ m n) 26)) 
			(map 'list #'c2i word)
        )
    )
)
;------------------------------------------------
(defun encode-paragraph (n paragraph)
	(cond 
		((null paragraph) '())
		(T (cons (encode-word n (car paragraph)) (encode-paragraph n (cdr paragraph))))
    )
)
;------------------------------------------------
(defun encode-document (n document)
	(cond 
		((null document) '())
		(T (cons (encode-paragraph n (car document)) (encode-document n (cdr document))))
    )
)
;; -----------------------------------------------
