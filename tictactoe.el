;; a nots and crosses game in lisp
;; probably terrible code as its a first time in lisp

(defun createPoint (x y val)
  "create a point and return it"
  (vector x y val))

(defun ptGetVal (pt)
  "get the value"
  (let ((VAL 2))
    (aref pt VAL)))

(defun ptGetX (pt)
  "get the value"
  (let ((X 0))
    (aref pt X)))

(defun ptGetY (pt)
  "get the value"
  (let ((Y 1))
    (aref pt Y)))

(defun ptSetVal (pt val)
  "set the value of a point"
  (let ((VAL 2)
	(pttmp pt)
	(result nil)
	)
    (aset pttmp VAL val)
    (setq result pttmp)
    )
  )

(defun ptCopy (pt)
  (createPoint (ptGetX pt) (ptGetY pt) (ptGetVal pt))
)

(defun brdCopy (brd)
  (let
      ((result nil)
       (x 0)
       )
    (setq result (createBoard))
    (while (< x 9)
      (aset result x (ptCopy (aref brd x)))
      (setq x (+ x 1))
      )
    (setq result result)
    )
)

(defun brdGetIndex (pt)
  "return the index in the board for a pt"
  (+ (ptGetX pt) (* (ptGetY pt) 3))
  )


(defun createBoard ()
  "create an empty board"
  (let (
	(y 0)
	(x 0)
	(v (list))
	)
    (while (< y 3)
      (while (< x 3)
	(setq v (append v (list (createPoint x y ?-))))
	(setq x (+ x 1))
	)
      (setq y (+ y 1))
      (setq x 0)
      )
    (vconcat v)
    )
  )
	
(defun printBoard (brd)
  "print the board in a neat format"
  (let (
	(index 0)
	)
    (while (< index 9)
      (princ (format "%c " (ptGetVal (aref brd index))))
      (setq index (+ index 1))
      (if (eq (% index 3) 0)
	  (princ "\n")
	nil)
      )
    )
  )

(defun makeMove (brd pt toPlay)
  "make a move if possible return the new brd"
  (let 
      ((result (brdCopy brd))
       (pttmp (ptCopy pt))
       )
    (if (eq (ptGetVal pttmp) ?-)
	(let ()
	  (aset result (brdGetIndex pttmp) (ptSetVal pttmp  toPlay))
	  (list result)
	  )
      nil
      )
    )
  )
	      
(defun possiMove (brd toPlay)
  "returns a list of possible paths of play"
  (delq nil (mapcar '(lambda(x) (makeMove brd x toPlay)) brd))
)

(defun makeNextMove (movList toPlay)
  (let
      ((finalresult (list)))
    (dolist (head movList finalresult)
      (setq finalresult 
	    (append (mapcar '(lambda(x)
			     (if (not (eq (length x) 0))
				 (append head x)
			       nil
			       ) 
			     )
			  (possiMove (car (last head)) toPlay)
			  ) 
		  finalresult
		  )
	    )
      (delq nil finalresult)
      )
    )
  )

(defun checkWin (brd)
  "looks for a win"
  ;; look for a diagonal
  (let
      ((default  ?-))
    (if (and (eq (ptGetVal (aref brd 0)) (ptGetVal (aref brd 4))) 
	     (eq (ptGetVal (aref brd 4)) (ptGetVal (aref brd 8)))
	     (not (eq (ptGetVal (aref brd 0)) ?-))
		 )
	(ptGetVal(aref brd 0))
      ;;look for next diagonal
      (if (and (eq (ptGetVal(aref brd 2)) (ptGetVal(aref brd 4))) 
	       (eq (ptGetVal(aref brd 4)) (ptGetVal(aref brd 6)))
	       (not (eq (ptGetVal(aref brd 2)) ?-))
	       )
	  (ptGetVal(aref brd 2))
	  ;;top row
	  (if (and (eq (ptGetVal(aref brd 0)) (ptGetVal(aref brd 1))) 
		   (eq (ptGetVal(aref brd 1)) (ptGetVal(aref brd 2)))
		   (not (eq (ptGetVal(aref brd 0)) ?-))
		   )
	      (ptGetVal(aref brd 0))
	;;middle row
	      (if (and (eq (ptGetVal(aref brd 3)) (ptGetVal(aref brd 4))) 
		       (eq (ptGetVal(aref brd 4)) (ptGetVal(aref brd 5)))
		       (not (eq (ptGetVal(aref brd 3)) ?-))
		       )
		  (ptGetVal(aref brd 3))
		  ;;bottom row
		  (if (and (eq (ptGetVal(aref brd 6)) (ptGetVal(aref brd 7))) 
			   (eq (ptGetVal(aref brd 7)) (ptGetVal(aref brd 8)))
			   (not (eq (ptGetVal(aref brd 6)) ?-))
			   )
		      (ptGetVal(aref brd 6))
		      ;;first column
		      (if (and (eq (ptGetVal(aref brd 0)) (ptGetVal(aref brd 3))) 
			       (eq (ptGetVal(aref brd 3)) (ptGetVal(aref brd 6)))
			       (not (eq (ptGetVal(aref brd 0)) ?-))
			       )
			  (ptGetVal(aref brd 0))
			  ;;second column
			  (if (and (eq (ptGetVal(aref brd 1)) (ptGetVal(aref brd 4))) 
				   (eq (ptGetVal(aref brd 4)) (ptGetVal(aref brd 7)))
				   (not (eq (ptGetVal(aref brd 1)) ?-))
				   )
			      (ptGetVal(aref brd 1))
			      ;;third column
			      (if (and (eq (ptGetVal(aref brd 2)) (ptGetVal(aref brd 5))) 
				       (eq (ptGetVal(aref brd 5)) (ptGetVal(aref brd 8)))
				       (not (eq (ptGetVal(aref brd 2)) ?-))
				       )
				  (ptGetVal(aref brd 2))
				  default
				  )
			      )
			  )
		      )
		  )
	      )
	  )
      )
    )
  )

(defun checkOver (brd)
  "all places used up"
  (let
      ((x 0)
       (result nil)
       )
    (while (< x 9)
      (setq result (or result (eq (ptrGetVal (aref brd x)) ?-)))
      (setq x (+ 1 x))
      )
    (setq result (not result))
    )
  )

(defun gameComplete (movList)
  "determines if a movList is complete"
  (or (checkWin ( car (last movList)))  (checkOver( car (last movList))))
  )

(defun printPath (path)
  "prints one path"
  (let
      ((result)
       )
    (dolist (element path result)
      (printBoard element)
      (princ "======\n")
      )
    )
  )

(defun runSim ()
  "Run the simulation"
  (let
      ((brd)
       )
    (setq brd (createBoard))
    (setq first (possiMove brd ?x))
    (setq final (makeNextMove first ?o))
    (print second)
    (printPath (car (makeNextMove first ?o)))
    )
  )