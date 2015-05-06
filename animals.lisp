;;;;;; USE (play) TO START THE ANIMAL GAME ;;;;;;

;;; Default animals to be used
(setf animals '("Does it bark?" "horse" "dog"))
(setf inp "-")

;;; YES- Right
;;; NO - Left

;;; To navigate through the tree
(defun move-right (node)
	(car (cdr (cdr node))))

(defun move-left (node)
	(car (cdr node)))

(defun navigate(node)
	(let ((parent node))
	(if
	(y-or-n-p (car node))
		(check-if-answer parent (move-right node))
		(check-if-answer parent (move-left node))))
)

;;; Check if the actual node is an animal
(defun check-if-answer(parent node)
	(if (listp node)
		(navigate node)
		(is-it-a parent node))
)


;;; Ask user if it is 'x' animal
;;; If it is, the animal was guessed
;;; If not, ask which was it and a differentiating question
(defun is-it-a(parent oldanimal)
	(if
	(y-or-n-p "Is it ~a? " (add-art oldanimal))
		(format t "I guessed your animal, I won!!")
		(let ((newanimal (ask-for-animal)))
			(what-would-be (insert parent (ask-diff-quest oldanimal newanimal) oldanimal) newanimal oldanimal)))
)

;;; Ask question to distinguish animals
;;; The insertion of the new animal will depend if the answer to the quest is y/n
(defun what-would-be(nodes newani oldani)
	(if
	(y-or-n-p "For ~a the answer would be? " (add-art newani))
		(side nodes newani oldani 1)
		(side nodes newani oldani 0)
	)
)

;;; Insertion will depend if it is on the right/left side
(defun side(node newanimal oldanimal answer)
	(if (listp (car node))
		(if (equal (nth 1 (car node)) oldanimal)
			;;modificar el lado izq
			(insertLeft node newanimal oldanimal answer)
			;;modificar lado derecho
			(insertRight node newanimal oldanimal answer)
		)
		;;modificar lado derecho
		(insertRight node newanimal oldanimal answer)
	)
)

(defun insertLeft(parent newanimal oldanimal answ)
	(if (equal answ 1)
		(rplacd (car parent) (list oldanimal newanimal))
		(rplacd (car parent) (list newanimal oldanimal))
	)
)
(defun insertRight(parent newanimal oldanimal answ)
	(if (equal answ 1)
		(rplacd (nth 1 parent) (list oldanimal newanimal))
		(rplacd (nth 1 parent) (list newanimal oldanimal))
	)
)

(defun insert(parent quest animal)
	(if (equal (nth 1 parent) animal)
		(rplaca (cdr parent) (list quest animal))
		(rplacd (cdr parent) (list (list quest animal)))
	)
)

(defun ask-for-animal()
	(remove-art (get-string "What was your animal?~%"))
)
	
;;; Series of functions to correct (if needed) the question typed by the user
(defun ask-diff-quest(oldAni newAni)
	(correct (get-string "What question would distinguish ~a from ~a?~%" (add-art oldAni) (add-art newAni))))

(defun correct(quest)
	(questmark (capitalize (string-downcase quest))))
	
(defun capitalize(txt)
	(string-capitalize txt :end 1))
	
(defun questmark(txt)
	(if (search "?" txt)
	(concatenate 'string txt "")
	(concatenate 'string txt "?")))
	
	
;;; Functions to handle articles	
(defun remove-art(animal)
	(let ((ani (string-downcase (string-trim '(#\space) animal))))
	(cond
		((search "a " ani) (subseq ani 2 (length ani)))
		((search "an " ani) (subseq ani 3 (length ani)))
		(T ani)))
)

(defun add-art(animal)
	(let ((first (subseq animal 0 1)))
	(if (or (equal first "a") (equal first "e") (equal first "i") (equal first "o") (equal first "u"))
		(concatenate 'string "an " animal)
		(concatenate 'string "a " animal)))
)

;;; Ask user for input and return it
(defun get-string (fmt &rest args)
  (apply #'format t fmt args)
  (read-line t))

	
(defun play-game()
	(get-string "~%Think of an animal and press enter!~%")
	(navigate animals)
	(play-again)
)

(defun play-again()
	(if
	(y-or-n-p "~%Do you want to play again? ")
		(play-game)
		(want-to-save))
)

(defun want-to-save()
	(if
	(y-or-n-p "Do you want to save your animals? ")
		(save)
		(end))
)

(defun save()
	(write-file)
	(end)
)

;;; Start the game and ask user if he wants to use a stored file
(defun initialize()
	(format t "Welcome to the animal game!!~%~%")
	(if
	(y-or-n-p "Do you want to load a file? ")
		(if (open-file)
			(load-animals)
			(file-not-found))
		(play-game)))

(defun play()
	(initialize)
)

(defun which-file()
	(get-string "Open which file? "))

;;; Open a file and save content into variable 'inp'
(defun open-file()
	(setf inp "h")
	(let ((in (open (which-file) :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (setf inp (concatenate 'string inp line)))
    (close in)))
)

;;; Strip the first and last parenthesis of 'inp' variable
;;; Overwrite 'animals variable' and start playing
(defun load-animals()
	(setf animals (string-to-list (subseq inp 2 (- (length inp) 1))))
	(play-game))

;;; If file was not found, start game with default animals
(defun file-not-found()
	(format t "File not found, using default animals...~%")
	(play-game))
	
(defun save-file()
	(get-string "~%Save in which file? "))

(defun end()
	(format t "Game finished! Thanks for playing!~%"))

;;;Write the content of 'animals' into the indicated file
(defun write-file()
	(with-open-file (outfile (save-file) :direction :output)
    (prin1 animals outfile)))

;;; Convert a string to a list
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

