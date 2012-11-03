(defun deleteSimilar (theseq similar)
  (delq nil (mapcar similar theseq))
  )
  
(setq theset (list (list (vector 1 2 3) (vector 4 5 3) (vector 6 7 3))
		   (list (vector 1 2 4) (vector 4 5 4) (vector 6 7 3))
		   (list (vector 1 2 5) (vector 4 5 5) (vector 6 7 9))))

(defun comparevect (x y)
  (if (and (eq (aref x 0) (aref y 0))
	   (eq (aref x 1) (aref y 1))
	   (eq (aref x 2) (aref y 2)))
      t
    nil
    )
  )


(defun runTest ()
  (deleteSimilar theset '(lambda(x)
			   (if (not (comparevect (car (last x)) 
						 (car (last (car (last theset))))))
			       x
			     nil
			     )
			   )
		 )
  )
