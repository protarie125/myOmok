(setf NoL 10) ;;Number of Lines
(setf mainBoard (make-array (list NoL NoL)))
(setf bip #\┼) ;;blank

;;board initializer and initialize
(defun initializeBoard ()
  (do ((i 0 (1+ i)))
      ((= i NoL) nil)
      (do ((j 0 (1+ j)))
	  ((= j NoL) nil)
	  (setf (aref mainBoard i j) bip))))
(initializeBoard)

;;board drawing function
(defun drawBoard (bo)
  (format t " 0123456789~%")
  (do ((i 0 (1+ i)))
      ((= i NoL) nil)
      (format t "~D" i)
      (do ((j 0 (1+ j)))
	  ((= j NoL) nil)
	  (format t "~A" (aref bo j i)))
      (format t "~D~%" i))
  (format t " 0123456789~%"))

;;set player's color, O/X
(defun getPlayerColor ()
  (let ((r nil))
    (loop
     (if r (return r))
     (format t "Do you want to be O or X?~%")
     (setf r (read))
     (if (or (eq r 'O) (eq r 'X))
	 (format t "You will be ~A~%" r)
       (progn
	 (format t "Ask agian: ")
	 (setf r nil))))))

;;make random state to get starting player randomly
(setf *random-state* (make-random-state t))
(defun getStartingPlayer ()
  (if (= (random 2) 0)
      'computer
    'player))

;;ask play again
(defun playAgain ()
  (format t "Do you want to play again? (yes or no)~%")
  (if (eq (read) 'yes)
      t
    nil))

;;actualy make move on "bo(board)" for "color(X/O)", at "move(cons)"
(defun makeMove (bo color move)
  (setf (aref bo (car move) (cdr move)) color))

;;check "color" is winner or not
(defun isWinner (bo color)
  (let (nc)
    (if (eq color 'O)
	(setf nc 'X)
      (setf nc 'O))
    (do ((i 0 (1+ i)))
	((= i NoL) nil)
	(do ((j 0 (1+ j)))
	    ((= j NoL) nil)
	    (cond
	     ((and (< j (- NoL 4))
		   (eq (aref bo i j) (aref bo i (+ j 1)))
		   (eq (aref bo i j) (aref bo i (+ j 2)))
		   (eq (aref bo i j) (aref bo i (+ j 3)))
		   (eq (aref bo i j) (aref bo i (+ j 4)))
		   (eq (aref bo i j) color)
		   (or (and (< j (- NoL 5))
			    (eq (aref bo i (+ j 5)) bip))
		       (= (+ j 5) NoL)))
	      (return-from isWinner t))
	     ((and (< i (- NoL 4))
		   (eq (aref bo i j) (aref bo (+ i 1) j))
		   (eq (aref bo i j) (aref bo (+ i 2) j))
		   (eq (aref bo i j) (aref bo (+ i 3) j))
		   (eq (aref bo i j) (aref bo (+ i 4) j))
		   (eq (aref bo i j) color))
	      (return-from isWinner t))
	     ((and (and (< i (- NoL 4)) (< j (- NoL 4)))
		   (eq (aref bo i j) (aref bo (+ i 1) (+ j 1)))
		   (eq (aref bo i j) (aref bo (+ i 2) (+ j 2)))
		   (eq (aref bo i j) (aref bo (+ i 3) (+ j 3)))
		   (eq (aref bo i j) (aref bo (+ i 4) (+ j 4)))
		   (eq (aref bo i j) color))
	      (return-from isWinner t))
	     ((and (and (< 3 i) (< 3 j))
		   (eq (aref bo i j) (aref bo (- i 1) (- j 1)))
		   (eq (aref bo i j) (aref bo (- i 2) (- j 2)))
		   (eq (aref bo i j) (aref bo (- i 3) (- j 3)))
		   (eq (aref bo i j) (aref bo (- i 4) (- j 4)))
		   (eq (aref bo i j) color))
	      (return-from isWinner t))
	     ((and (and (< 3 i) (< j (- NoL 4)))
		   (eq (aref bo i j) (aref bo (- i 1) (+ j 1)))
		   (eq (aref bo i j) (aref bo (- i 2) (+ j 2)))
		   (eq (aref bo i j) (aref bo (- i 3) (+ j 3)))
		   (eq (aref bo i j) (aref bo (- i 4) (+ j 4)))
		   (eq (aref bo i j) color))
	      (return-from isWinner t))
	     ((and (and (< 3 j) (< i (- NoL 4)))
		   (eq (aref bo i j) (aref bo (+ i 1) (- j 1)))
		   (eq (aref bo i j) (aref bo (+ i 2) (- j 2)))
		   (eq (aref bo i j) (aref bo (+ i 3) (- j 3)))
		   (eq (aref bo i j) (aref bo (+ i 4) (- j 4)))
		   (eq (aref bo i j) color))
	      (return-from isWinner t))
	     (t nil))))))

;;check "4 in a line" function
(defun isMaybeWinner (bo color)
  (do ((i 0 (1+ i)))
      ((= i NoL) nil)
      (do ((j 0 (1+ j)))
	  ((= j NoL) nil)
	  (cond
	   ((and (< j (- NoL 4))
		 (eq (aref bo i j) (aref bo i (+ j 1)))
		 (eq (aref bo i j) (aref bo i (+ j 2)))
		 (eq (aref bo i j) (aref bo i (+ j 3)))
		 (eq (aref bo i j) color)
		 (or (and (< j (- NoL 5))
			  (eq (aref bo i (+ j 5)) bip))
		     (= (+ j 5) NoL)))
	    (return-from isMaybeWinner t))
	   ((and (< i (- NoL 4))
		 (eq (aref bo i j) (aref bo (+ i 1) j))
		 (eq (aref bo i j) (aref bo (+ i 2) j))
		 (eq (aref bo i j) (aref bo (+ i 3) j))
		 (eq (aref bo i j) color))
	    (return-from isMaybeWinner t))
	   ((and (and (< i (- NoL 4)) (< j (- NoL 4)))
		 (eq (aref bo i j) (aref bo (+ i 1) (+ j 1)))
		 (eq (aref bo i j) (aref bo (+ i 2) (+ j 2)))
		 (eq (aref bo i j) (aref bo (+ i 3) (+ j 3)))
		 (eq (aref bo i j) color))
	    (return-from isMaybeWinner t))
	   ((and (and (< 3 i) (< 3 j))
		 (eq (aref bo i j) (aref bo (- i 1) (- j 1)))
		 (eq (aref bo i j) (aref bo (- i 2) (- j 2)))
		 (eq (aref bo i j) (aref bo (- i 3) (- j 3)))
		 (eq (aref bo i j) color))
	    (return-from isMaybeWinner t))
	   ((and (and (< 2 i) (< j (- NoL 4)))
		 (eq (aref bo i j) (aref bo (- i 1) (+ j 1)))
		 (eq (aref bo i j) (aref bo (- i 2) (+ j 2)))
		 (eq (aref bo i j) (aref bo (- i 3) (+ j 3)))
		 (eq (aref bo i j) color))
	    (return-from isMaybeWinner t))
	   ((and (and (< 3 j) (< i (- NoL 4)))
		 (eq (aref bo i j) (aref bo (+ i 1) (- j 1)))
		 (eq (aref bo i j) (aref bo (+ i 2) (- j 2)))
		 (eq (aref bo i j) (aref bo (+ i 3) (- j 3)))
		 (eq (aref bo i j) color))
	    (return-from isMaybeWinner t))
	   (t nil)))))

;;get copy of board func.
(defun getBoardCopy (bo)
  (let ((dupeBoard (make-array (list NoL NoL))))
    (do ((i 0 (1+ i)))
	((= i NoL) dupeBoard)
	(do ((j 0 (1+ j)))
	    ((= j NoL) nil)
	    (setf (aref dupeBoard i j) (aref bo i j))))))

;;check the point "move" of "bo" is free or not
(defun isSpaceFree (bo move)
  (if (eq (aref bo (car move) (cdr move)) bip)
      t
    nil))

;;input func. for player
(defun getPlayerMove (bo)
  (let ((r nil))
    (loop
     (if r (return r))
     (format t "What is your next move?~%(For example, input (9 . 0) to move to top right corner.~%")
     (setf r (read))
     (if (and (consp r) (isSpaceFree bo r))
	 (format t "You moved to ~A~%" r)
       (progn
	 (format t "Ask again: ")
	 (setf r nil))))))

;;get computer's move algorithm
(defun getComputerMove (bo comcol)
  (let (playercol (move nil))
    (if (eq comcol 'x)
	(setf playercol 'O)
      (setf playercol 'X))
    (loop
     (if move (return move))
     ;;1. winning point
     (do ((i 0 (1+ i)))
	 ((= i NoL) move)
	 (do ((j 0 (1+ j)))
	     ((= j NoL) move)
	     (let ((copy (getBoardCopy bo)))
	       (cond
		((isSpaceFree copy (cons i j))
		 (makeMove copy comcol (cons i j))
		 (if (isWinner copy comcol)
		     (progn
		       (setf move (cons i j))
		       (return-from getComputerMove move))))))))
     ;;2. block the players winning move
     (do ((i 0 (1+ i)))
	 ((= i NoL) move)
	 (do ((j 0 (1+ j)))
	     ((= j NoL) move)
	     (let ((copy (getBoardCopy bo)))
	       (cond
		((isSpaceFree copy (cons i j))
		 (makeMove copy playercol (cons i j))
		 (if (isWinner copy playercol)
		     (progn
		       (setf move (cons i j))
		       (return-from getComputerMove move))))))))
     ;;3. block the players "4-in-a-line" move
     (do ((i 0 (1+ i)))
	 ((= i NoL) move)
	 (do ((j 0 (1+ j)))
	     ((= j NoL) move)
	     (let ((copy (getBoardCopy bo)))
	       (cond
		((isSpaceFree copy (cons i j))
		 (makeMove copy playercol (cons i j))
		 (if (isMaybeWinner copy playercol)
		     (progn
		       (setf move (cons i j))
		       (return-from getComputerMove move))))))))
     ;;4. move to center
     (if (isSpaceFree mainBoard (cons 5 5))
	 (setf move (cons 5 5)))
     ;;5. move to around of center (3*3)
     (loop
      (if move (return move))
      (let (x y)
	(setf x (+ 4 (random 3)))
	(setf y (+ 4 (random 3)))
	(if (isSpaceFree bo (cons x y))
	    (setf move (cons x y)))))
     ;;6. move to around of center (5*5)
     (loop
      (if move (return move))
      (let (x y)
	(setf x (+ 3 (random 5)))
	(setf y (+ 3 (random 5)))
	(if (isSpaceFree bo (cons x y))
	    (setf move (cons x y)))))
     ;;7. move randomly
     (loop
      (if move (return move))
      (let (x y)
	(setf x (random NoL))
	(setf y (random NoL))
	(if (isSpaceFree bo (cons x y))
	    (setf move (cons x y))))))))

;;check the board is full or not (to check the game is a tie)
(defun isBoardFull (board)
  (do ((i 0 (1+ i)))
      ((= i NoL) t)
      (do ((j 0 (1+ j)))
	  ((= j NoL) nil)
	  (if (isSpaceFree board (cons i j))
	      (return-from isBoardFull nil)))))

;;main
(format t "Welcome to Omok!~%")
(loop
 (if nil (return))
 (initializeBoard)
 ;;set players' color
 (setf PC (getPlayerColor))
 (if (eq PC 'O)
     (setf CC 'X)
   (setf CC 'O))
 ;;set starter
 (setf turn (getStartingPlayer))
 (format t "The ~A will go first.~%" turn)
 ;;go game
 (setf gameIsPlaying t)
 (loop
  (if (null gameIsPlaying) (return))
  (cond
   ((eq turn 'player)
    (drawBoard mainBoard)
    (let ((move (getPlayerMove mainBoard)))
      (makeMove mainBoard PC move)
      (cond
       ((isWinner mainBoard PC) ;;#1 winning check
	(drawBoard mainBoard)
	(format t "You have won the game!~%")
	(setf gameIsPlaying nil))
       ((isBoardFull mainBoard) ;;#2 tie check
	(drawBoard mainBoard)
	(format t "The game is a tie!~%")
	(setf gameIsPlaying nil))
       (t
	(setf turn 'computer))))) ;;#3 give turn
   (t
    (let ((move (getComputerMove mainBoard CC)))
      (makeMove mainBoard CC move)
      (cond
       ((isWinner mainBoard CC)
	(drawBoard mainBoard)
	(format t "The computer has beaten you! You lose.~%")
	(setf gameIsPlaying nil))
       ((isBoardFull mainBoard)
	(drawBoard mainBoard)
	(format t "The game is a tie!~%")
	(setf gameIsPlaying nil))
       (t
	(setf turn 'player)))))))

 ;;ask play again
 (if (playAgain)
     nil
   (return nil))) ;;force return from loop
