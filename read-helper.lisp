
;------------------------------------------------------------
; Reads all the lines from input opened file descriptor
(defun read-lines (fd)
	(setq line (read-line in nil))
	(if (null line)
		line
		(if (equal (length line) 0)
			(read-lines fd)
			(cons line (read-lines fd))
		)
	)
)
;------------------------------------------------------------
(defun clean (list)
	(cond
		((null list) nil)
		((or (null (car list)) (equal 0 (length (car list)))) (clean (cdr list)))
		(T (cons (car list) (clean (cdr list))))
	)
)
;------------------------------------------------------------
(defun helper-parse (list)
	(if (null list)
		list
		(append (clean  (list (car list))) (helper-parse (cdr list)))
	)
)
;------------------------------------------------------------
(defun parse (list)
	(if (null list)
		list
		(append (helper-parse (clean (delete-whites (car list)))) (parse (cdr list)))
	)
)
;------------------------------------------------------------
(defun delete-whites (line)
	(cond 
		((null line) line)
		(T
			(setq startp 0)
			(setq pos_space (position #\SPACE line :test #'equal))
			(setq pos_tab (position #\TAB line :test #'equal))
			(cond
				((and (null pos_space) (null pos_tab)) (list line))
				((null pos_space) (cons (subseq line startp pos_tab) (delete-whites (subseq line (+ 1 pos_tab)))))
				((null pos_tab) (cons (subseq line startp pos_space) (delete-whites (subseq line (+ 1 pos_space)))))
				((> pos_space pos_tab) (cons (subseq line startp pos_tab) (delete-whites (subseq line (+ 1 pos_tab)))))
				(T (cons (subseq line startp pos_space) (delete-whites (subseq line (+ 1 pos_space)))))
			)
		)
	)
)
;------------------------------------------------------------