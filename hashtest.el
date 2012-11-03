
(setq po (list (vector (vector 0 0 120) (vector 1 0 45) (vector 2 0 45) 
		       (vector 0 1 45) (vector 1 1 45) (vector 2 1 45) 
		       (vector 0 2 45) (vector 1 2 45) (vector 2 2 45)) 
	       (vector (vector 0 0 120) (vector 1 0 45) (vector 2 0 45) 
		       (vector 0 1 45) (vector 1 1 45) (vector 2 1 45) 
		       (vector 0 2 45) (vector 1 2 45) (vector 2 2 111))))
(setq pi (list (vector (vector 0 0 45) (vector 1 0 120) (vector 2 0 45) 
		       (vector 0 1 45) (vector 1 1 45) (vector 2 1 45) 
		       (vector 0 2 45) (vector 1 2 45) (vector 2 2 45)) 
	       (vector (vector 0 0 120) (vector 1 0 45) (vector 2 0 45) 
		       (vector 0 1 45) (vector 1 1 45) (vector 2 1 45) 
		       (vector 0 2 45) (vector 1 2 45) (vector 2 2 111))))

(defun pathhash (path)
  (apply 
   'concat (mapcar 
	    '(lambda (pt)
	       (apply 'concat (mapcar 'number-to-string pt))
	       ) 
	    (car (last path))
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

(pathhash= pi po)
(pathhash pi)
(pathhash po)
	       
	    
pi
(number-to-string ?x)
