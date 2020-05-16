
(load "include.lisp") 

(load "encode.lisp")

(load "test-dictionary.lisp") 

(load "read-helper.lisp")



;------------------------------------------------------



(defun read-as-list (filename)

	(setq in (open filename :if-does-not-exist nil))

	(cond 

		((null in) (write "No such file") nil)

		(T (setq all (read-lines in)) 

			(close in)

			(parse (clean all)))

	)

)



;------------------------------------------------------

; helper for make list

(defun smalllist (l n) 

   (when l 

      (cons (subseq l 0 (min n (length l))) 

            (smalllist (nthcdr n l) n))))

;-------------------------------------------------------

(defun combine-paragraph (document)

	(cond 

		((null document) '())

		(T (append (car document) (combine-paragraph (cdr document))))

    )

)

;; -----------------------------------------------------



;; -----------------------------------------------------

;; HELPERS

;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;; -----------------------------------------------------

(defun helper-spell-checker-0 (word list)

	(if (not (null list))

		(dolist (n list)

			(if (equal word n)(return-from helper-spell-checker-0 t)))

		nil

	)

)

;; -----------------------------------------------------

; brute force checks the whether the word occurs in the dictionary or not

(defun spell-checker-0 (word)

	(helper-spell-checker-0 word *dictionary*)

)

(defun spell-checker-1 (word)

 	;you should implement this function

)

;; -----------------------------------------------------

; check the given char is in list or not

(defun helper-map-word (ch list)

	(dolist (n list)

		(if (equal ch (cadr n))

			(return-from helper-map-word t)

		)

	)



	(return-from helper-map-word nil)

)

;; -----------------------------------------------------

; it finds where is the last difference index

(defun find-last-difference (list)

	(let ((ind 0))



	(dolist (n list)

		(if (< ind (caddr n)) (setf ind (caddr n)))

	)



	(return-from find-last-difference ind))

)

;; -----------------------------------------------------

; it converts a ch to another one, it gives another index to the char

(defun convert-char (ch ind list)



	(let ((diff (find-last-difference list)))



	(dolist (n list)

		(if (and (equal ch (car n))(equal '_ (cadr n)))

			(progn 

				(setf (cadr n) ind)

				(setf (caddr n) (+ diff 1))

				(return-from convert-char t)

			)

		)

	)



	(return-from convert-char nil))

)

;; -----------------------------------------------------

; make uncoverted version of the given list

(defun undo-convert (ind list)

	(dolist (n list)

		(if (< ind (caddr n))

			(progn

				(setf (caddr n) 0)

				(setf (cadr n) '_)

			)



		)

	)

)

;; -----------------------------------------------------

; getter of mapped char

(defun mapped-char (ch list)

	(dolist (n list)

		(if (equal ch (car n))

			(return-from mapped-char (cadr n))))

)

;; -----------------------------------------------------

; getter of original version of mapped one

(defun original-char (ch list)

	(dolist (n list)

		(if (equal ch (cadr n))

			(return-from original-char (car n))))

)

;; -----------------------------------------------------

;; mapping btw encoded and dictionary word

(defun encode-map-dictionary (encoded_word dic list)



	(let ((diff (find-last-difference list)))

	(dolist (n encoded_word)



		(setf first-elm (car dic))		

		(setf dic (cdr dic))	

		(setf value (mapped-char n list))



		(if(and (helper-map-word first-elm list)

		   (not (equal (original-char first-elm list) n)))

			(progn

				(undo-convert diff list)

				(return-from encode-map-dictionary nil)	

			)	

		)



		(cond

			((equal value '_) (convert-char n first-elm list))

			((equal value first-elm) (continue))

			(t  (progn

					(undo-convert diff list)

					(return-from encode-map-dictionary nil)

				)

			)

		)

	)



	(return-from encode-map-dictionary t))

)

;; -----------------------------------------------------

(defun helper-for-decoder (paragraph list)



	(let ((first-elem (car paragraph)) (others (cdr paragraph)))

	(if (null first-elem) (return-from helper-for-decoder t))

	(let ((diff (find-last-difference list)))

	

	(dolist (dic *dictionary*)

		(if (equal (length first-elem)(length dic))

			(progn

				(if (encode-map-dictionary first-elem dic list)

					(progn

						(if(helper-for-decoder others list)

							(return-from helper-for-decoder t)

						)

					)

					(progn

						(undo-convert diff list)

						(continue)

					)

				)

			)

		)

	)

	(return-from helper-for-decoder nil)

	))

)

;;------------------------------------------------------

;; HELPERS FOR GEN-DECODER-B

; finds occurence a letter is in how many time in a word

(defun occur-word (word letter)

	(let ((first-elem (car word)) (others (cdr word)))



        (if (null first-elem) (return-from occur-word 0))

        (if (equal first-elem letter)

            (return-from occur-word (+ 1 (occur-word others letter)))

            (return-from occur-word (+ 0 (occur-word others letter)))

        )

	)

)

;;------------------------------------------------------

;  finds occurence a letter is in how many time in a paragraph

(defun occur-paragraph (paragraph letter)

	(let ((first-elem (car paragraph)) (others (cdr paragraph)))



        (if (null first-elem) (return-from occur-paragraph 0))

        (return-from occur-paragraph (+ (occur-word first-elem letter) (occur-paragraph others letter)))

	)

)

;;------------------------------------------------------

(defun helper-gen-decoder-b0 (paragraph list)

	(dolist (current_letter list)

		(setf (cadddr current_letter) (occur-paragraph paragraph (car current_letter)))

    )

)

;;------------------------------------------------------

(defun helper-gen-decoder-b0-1 (mostFrequentLetters alphabetList)



	(if (null mostFrequentLetters) (return-from helper-gen-decoder-b0-1 t))



	(let ((mostFrequentLetter '(_)) (mostFrequentNumber 0)

		 (first-elem (car mostFrequentLetters)) (rest_letters (cdr mostFrequentLetters)))



		(dolist (letter alphabetList)

			(if (and (< mostFrequentNumber (cadddr letter)) (equal '_ (cadr letter)))

				(progn

					(setf mostFrequentNumber (cadddr letter))

					(setf mostFrequentLetter (car letter))

				)

			)

		)



		(convert-char mostFrequentLetter first-elem alphabetList)

		(helper-gen-decoder-b0-1 rest_letters alphabetList)

	)

)

;; -----------------------------------------------------

;; For making decode some helpers

(defun encoded-word (word list)

	(cond 

		((null word) '())

		(T (append (list (mapped-char (car word) list)) (encoded-word (cdr word) list)))

    )

)

;;------------------------------------------------------

(defun encoded-paragraph (paragraph list)

	(cond 

		((null paragraph) '())

		(T (cons (encoded-word (car paragraph) list) (encoded-paragraph (cdr paragraph) list)))

    )

)

;;-------------------------------------------------------

(defun encoded-document (document list)

	(cond 

		((null document) '())

		(T (cons (encoded-paragraph (car document) list) (encoded-document (cdr document) list))

        )

    )

)

;; ------------------------------------------------------

;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph)

	(let ((*alphabet* '(

				(a _ 0)(b _ 0)(c _ 0)(d _ 0)(e _ 0)(f _ 0)(g _ 0)(h _ 0)(i _ 0)(j _ 0)(k _ 0)(l _ 0)(m _ 0)

				(n _ 0)(o _ 0)(p _ 0)(q _ 0)(r _ 0)(s _ 0)(t _ 0)(u _ 0)(v _ 0)(w _ 0)(x _ 0)(y _ 0)(z _ 0)

			))

		)



		(helper-for-decoder paragraph *alphabet*)

		(return-from Gen-Decoder-A *alphabet*)

	)

)

;; ------------------------------------------------------

;; ------------------------------------------------------

(defun Gen-Decoder-B-0 (paragraph)

   	(let (	

		   (*most-frequent-letters* '(e t a o i n))

		   (*alphabet* '(

				(a _ 0 _)(b _ 0 _)(c _ 0 _)(d _ 0 _)(e _ 0 _)(f _ 0 _)(g _ 0 _)(h _ 0 _)(i _ 0 _)(j _ 0 _)(k _ 0 _)(l _ 0 _)(m _ 0 _)

				(n _ 0 _)(o _ 0 _)(p _ 0 _)(q _ 0 _)(r _ 0 _)(s _ 0 _)(t _ 0 _)(u _ 0 _)(v _ 0 _)(w _ 0 _)(x _ 0 _)(y _ 0 _)(z _ 0 _)

			))	

		) 

	

		(helper-gen-decoder-b0 paragraph *alphabet*)

		(helper-gen-decoder-b0-1 *most-frequent-letters* *alphabet*) 

		(helper-for-decoder paragraph *alphabet*)

		(return-from Gen-Decoder-B-0 *alphabet*)

	)

)

;; ------------------------------------------------------

;; --------------------------------------------------------------------------------------------

(defun Gen-Decoder-B-1 (paragraph)

	(let ((*alphabet* '(

			(a _ 0)(b _ 0)(c _ 0)(d _ 0)(e _ 0)(f _ 0)(g _ 0)(h _ 0)(i _ 0)(j _ 0)(k _ 0)(l _ 0)(m _ 0)

			(n _ 0)(o _ 0)(p _ 0)(q _ 0)(r _ 0)(s _ 0)(t _ 0)(u _ 0)(v _ 0)(w _ 0)(x _ 0)(y _ 0)(z _ 0)

			)))





		(helper-for-decoder paragraph *alphabet*)

		(return-from Gen-Decoder-B-1 *alphabet*)

	)

)

;; ----------------------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------------------

(defun Code-Breaker (document decoder-function)

	(let ((docParagraph (combine-paragraph document)))

	(let ((list (funcall decoder-function docParagraph)))

	(return-from Code-Breaker (encoded-document document list))))

)

;; -----------------------------------------------------------------------------------------------

;; -----------------------------------------------------------------------------------------------

(defun test_on_test_data ()



	(format t "--------------------------------------------------------------~%")



    (format t "_________________Test 1 for spell-checker-0___________________~%")

    (format t "_________________(spell-checker-0 '(t e s t))_________________~%")

    (format t "~a~%~%" (spell-checker-0 '(t e s t)))





    (format t "_________________Test 2 for spell-checker-0_________________________________________~%")

    (format t "_________________(spell-checker-0 '(n o t i n d i c t i o n a r y))_________________~%")

    (format t "~a~%~%" (spell-checker-0 '(n o t i n d i c t i o n a r y)))







    (format t "_________________Test for read-as-list___________________________________________~%")

    (format t "_________________Inside of document1.txt_________________________________________~%")

    (defparameter *document1* (read-as-list "document1.txt"))

    (format t "~a~%~%" *document1*)







    (format t "_________________Test for read-as-list___________________________________________~%")

    (format t "_________________Inside of dictionary1.txt_______________________________________~%")

    (defparameter *dictionary1* (read-as-list "dictionary1.txt"))

    (format t "~a~%~%" *dictionary1*)







    (format t "_________________Test for read-as-list___________________________________________~%")

    (format t "_________________Inside of document1.txt_________________________________________~%")

    (defparameter *document1* (read-as-list "document1.txt"))

    (format t "~a~%~%" *document1*)







	(format t "_________________Test for read-as-list___________________________________________~%")

    (format t "_________________Inside of document2.txt_________________________________________~%")

    (defparameter *document2* (read-as-list "document2.txt"))

    (format t "~a~%~%" *document2*)

	



    (defparameter *test-document* '(

                    ((j u l y)(j u n e)(a n)(o r)(h e r))

                    ((t h i s)(c o v e r s)(a t o m)(o r)(m o d e l))

                    ))

  

  

   (defparameter *document* '(

                    ((h e l l o));;paragraph 1

                    ((t h i s)(i s)(t e s t));;paragraph 2

                    ))





	(defparameter *encoded-document-test* (encode-document 5 *document*)) 

    (format t "_________ENCODED DOCUMENT FOR TESTING - 1 ____________~%")

    (format t "~a~%~%" *encoded-document-test*)





    (format t "_________________Gen-Decoder-A TEST___________________~%")

	(format t "~a~%~%" (Code-Breaker *encoded-document-test* #'Gen-Decoder-A))





	(format t "_________________Gen-Decoder-B-0 TEST_________________~%")

	(format t "~a~%~%" (Code-Breaker *encoded-document-test* #'Gen-Decoder-B-0))



	



	(format t "_________________Gen-Decoder-B-1 TEST_________________~%")

	(format t "~a~%~%" (Code-Breaker *encoded-document-test* #'Gen-Decoder-B-1))



)

;; --------------------------------------------------------------------------------------------------



;; test code...

(test_on_test_data)
