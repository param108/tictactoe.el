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
	
(defun comparept (x y)
  (if (and (eq (aref x 0) (aref y 0))
	   (eq (aref x 1) (aref y 1))
	   (eq (aref x 2) (aref y 2)))
      t
    nil
    )
  )

(defun compareBoard (x y)
  (eq 0 (length (remq 
		 t
		 (mapcar 
		  '(lambda (xd)
		     (if (comparept xd (aref y (brdGetIndex xd)))
			 t
		       nil)) 
		  x)
		 )
		)
      )
  )
	
(defun deleteSimilarPaths (paths src)
  (remq nil (mapcar '(lambda(x) 
		     (if (not (compareBoard (car (last x)) (car (last src))))
			 x
		       nil
		       )
		     ) paths)
	)
  )

(defun pathhash (brd)
  (sxhash (apply 
	   'concat (mapcar 
		    '(lambda (pt)
		       (apply 'concat (mapcar 'number-to-string pt))
		       ) 
		    brd
		    )
	   )
	  )
  )

(defun pathhash= ( p1 p2 )
  (apply 'eq 
	 (mapcar 'pathhash 
		 (list p1 p2)
		 )
	 )
  )

(define-hash-table-test 'pathhashtable 'pathhash= 'pathhash)


(defun printBoard (brd)
  "print the board in a neat format"
  (let (
	(index 0)
	)
    (if (eq (length brd) 0)
	(print "the board is a nil")
      (while (< index 9)
	(princ (format "%c " (ptGetVal (aref brd index))))
	(setq index (+ index 1))
	(if (eq (% index 3) 0)
	    (princ "\n")
	  nil
	  )
	)
      )
    (print "~~~~~~")
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

(defun possiMove (brd toPlay ht)
  "returns a list of possible paths of play"
  (let 
      ((result))
    (if (not (eq nil (setq result (gethash brd ht))))
	result
      (let 
	  ()
	(setq result (winfilter (remq nil (mapcar '(lambda(x) (makeMove brd x toPlay)) brd))))
	(puthash brd result ht)
	result
	)
      )
    )
  )

(defun winfilter (brdlist)
  "filter out successful board if available, else return all"
  (let 
      ((result)
       )
    (dolist (brd brdlist result)
      (if (boardComplete (car brd))
	  (return (list brd))
	  nil
	  )
      )
    (setq result brdlist)
    )
  )


(defun winpaths (paths)
  (let
      ((wins (list))
       (win)
       (path)
       )
    (setq wins (remq nil 
		     (mapcar '(lambda(x)
				(if (boardComplete (car (last x)))
				    (setq win x)
				  (setq win nil)
				  )
				) 
			     paths
			     )
		     )
	  )
    )
  )
(defun nonwinpaths (paths)
  (let
      ((wins (list))
       (win)
       (path)
       )
    (setq wins (remq nil 
		     (mapcar '(lambda(x)
				(if (not (boardComplete (car (last x))))
				    (setq win x)
				  (setq win nil)
				  )
				) 
			     paths
			     )
		     )
	  )
    )
  )
		   
(defun makeNextMove (movList toPlay)
  (let
      ((finalresult (list))
       (solutions (list))
       (nonsolutions (list))
       (hashTable (make-hash-table :test 'pathhashtable))
       )
    (setq nonsolutions (nonwinpaths movList))
    (while (not (eq (length nonsolutions) 0))
      (setq finalresult (mapcar 
			 '(lambda (head)
			    (mapcar 
			     '(lambda(x)
				(append head x)
				)
			     (possiMove 
			      (car (last head)) toPlay hashTable)
			     )
			    ) 
			 nonsolutions
			 )
	    )
      ;; now flatten the list
      (setq finalresult (apply 'append finalresult))
      ;; clear the hashtable
      (clrhash hashTable)
      (if (eq toPlay ?x)
	  (setq toPlay ?o)
	(setq toPlay ?x)
	)
      (setq nonsolutions (nonwinpaths finalresult))
      (print (format "done %d" (length nonsolutions)))
      (setq solutions (append 
		       solutions 
		       (winpaths finalresult)
		       )
	    )
      )
    )
  )

(defun checkA3 (le mid re brd)
  "takes the indexes of the winning 3 some and returns the piece if a win
   else ?-"
  (if (and (eq (ptGetVal (aref brd le)) (ptGetVal (aref brd mid))) 
	   (eq (ptGetVal (aref brd mid)) (ptGetVal (aref brd re)))
	   (not (eq (ptGetVal (aref brd le)) ?-)))
      (ptGetVal (aref brd le))
    ?-
    )
  )
		   
(defun checkWin (brd)
  "looks for a win"
  (not (eq 0 (length 
	      (remq ?- (mapcar 
			'(lambda(x)
			   (checkA3 (nth 0 x) (nth 1 x) (nth 2 x) brd)
			   )
			(list (list 0 4 8) (list 2 4 6) (list 0 1 2) (list 3 4 5) 
			 (list 6 7 8) (list 0 3 6) (list 1 4 7) (list 2 5 8)))
		    )
	      )
	   )
       )
  )
  

(defun checkWinOld (brd)
  "looks for a win"
  ;; look for a diagonal
  (let
      ((default  ?-))
    (not (eq ?- (if (and (eq (ptGetVal (aref brd 0)) (ptGetVal (aref brd 4))) 
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
    )
  )
  
(defun checkOver (brd)
  "all places used up"
  (let
      ((x 0)
       (result nil)
       )
    (while (< x 9)
      (setq result (or result (eq (ptGetVal (aref brd x)) ?-)))
      (setq x (+ 1 x))
      )
    (setq result (not result))
    )
  )

(defun boardComplete (brd)
  "check if a board is complete"
  (or (checkWin brd)  (checkOver brd))
  )

(defun gameComplete (movList)
  "determines if a movList is complete"
  (or (checkWin ( car (last movList)))  (checkOver( car (last movList))))
  )

(defun gamesComplete (movLists)
  "determines if all possible games are complete"
  (let
      ((result t)
       (element)
       )
    (dolist (element movLists result)
      (setq result (and result (gameComplete element)))
      )
    (setq result result)
    )
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
(defun printAllPaths (paths)
  "prints one path"
  (let
      ((result)
       (result2)
       (num 0)
       )
    (dolist (path paths result)
      (dolist (element path result2)
	(if (eq (length element) 0)
	    (print "NULL ELEMENT FOUND")
	  (let ()
	    (printBoard element)
	    (princ "======\n")
	    )
	  )
	)
      (setq num (+ num 1))
      (princ (format "nextboard:%d\n" num))
      )
    )
  )
(defun printIfComplete (paths)
  "prints one path"
  (let
      ((result)
       (result2)
       (num 0)
       )
    (dolist (path paths result)
      (if (boardComplete (car (last path)))
	  (printBoard (car (last path)))
	nil
	)
      )
    )
  )

(defun runSim ()
  "Run the simulation"
  (let
      ((brd)
       (first)
       (final)
       )
    (setq brd (createBoard))
    (setq first (possiMove brd ?x (make-hash-table :test 'pathhashtable)))
    (setq final (makeNextMove first ?o))
  )
)