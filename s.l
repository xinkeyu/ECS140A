(defun isLL2(q)
    ;indicates whether q is a list of exactly length 2
    (cond ((not (listp q)) nil) ;if not a list, return nil
    	(t (cond ((and (not(null (cdr q))) (null (cddr q)) ) t) ;if cdr is not null and cddr is null
    			(t nil)
    		)
    	)
    )
)

(defun isLL2a(q)
    ;indicates whether q is a list of exactly length 2 and whether q's second element is a list
    (cond ((not (listp q)) nil) ;if not a list, return nil
    	(t (cond ((and (not(null (cdr q))) (null (cddr q)) (listp (cadr q))) t) ;if cdr is not null and cddr is null and cddr is a list
    			(t nil)
    		)
    	)
    )
)

(defun isLpairs-recursive(q)
	;recursively checks whether q is a list of pairs
	(cond ((not (listp q)) nil) ;if not a list, return nil
		  ((null q) t) ;reach the end of list, return true
		  ((not(isLL2 (car q))) nil) ;if the first list if a pair, return nil
		  (t (isLpairs-recursive (cdr q))) ;recurse on rest of the list
	)
)

(defun isLpairs-iterative(q)
	;iteratively checks whether q is a list of pairs
	(cond ((not (listp q)) nil) ;if not a list, return nil		
		(t 	(do ( (q1 q (cdr q1))  ;q1 is the rest of the list
				)
				((or (null q1) (not(isLL2 (car q1)))) (null q1)) ;stop if reach the end of list or encounters a nonpair
			)
		)
	)
)

(defun isLpairs-every(q)
	;use every to check whether q is a list of pairs

	(cond ((not (listp q)) nil) ;if not a list, return nil		  
		  (t (every #'isLL2 q)
		  )
	)
	
)

(defun isLpairs1st-recursive (q)
	;recursively checks if q is a list of pairs whose first elements are the same
	(cond ((not(isLpairs-recursive q)) nil);not a list of pairs
		  ((null (cdr q)) t);reach the end of list
		  ((not(equal (caar q) (caadr q))) nil);reach and unequal pair
		  (t (isLpairs1st-recursive (cdr q)));recurse on the rest of the list
	)
)

(defun isLpairs1st-iterative (q)
	;iteratively checks if q is a list of pairs whose first elements are the same
	(cond ((not(isLpairs-iterative q)) nil);not a list of pairs
		  (t (do ( (q1 q (cdr q1))
		  		 )
		  		 ((or (null (cdr q1)) (not(equal (caar q1) (caadr q1)))) (null (cdr q1)))
		  	 )
		  )
	)
)

(defun isLpairs1st-every (q)
	;use every to check if q is a list of pairs whose first elements are the same
	(defun compare(q1)
		(equal (caar q) (car q1))
	)
	(cond ((not (isLpairs-every q)) nil) ;not a list of pairs
		  ((null (car q)) t)
		  (t (every #'compare (cdr q))
		  )
	)
)

(defun distribute-recursive (q)
	;if q is not isLL2a, the function returns nil
	;otherwise, return a list of pairs formed from q's first element with each element of q's second element
	(cond ((not (isLL2a q)) nil) ;if not isLL2a
		  ((null (cadr q)) nil) ;end of cdr
		  (t (cons (list (car q) (caadr q)) (distribute-recursive (list (car q)(cdadr q)))) 
		  )
	)
)

(defun distribute-iterative (q)
	;if q is not isLL2a, the function returns nil
	;otherwise, return a list of pairs formed from q's first element with each element of q's second element
	(cond ((not (isLL2a q)) nil) ;if not isLL2a
		  (t (do (  (q1 q (list (car q1)(cdadr q1)))
		  			(res nil (append res (list(list (car q1) (caadr q1)))))
		      	 )
		      	 ((null (cadr q1)) res)
		  	 )
		  )
	)
)

(defun distribute-mapcar (q)
	;if q is not isLL2a, the function returns nil
	;otherwise, return a list of pairs formed from q's first element with each element of q's second element
	(defun distribute(q1)
		(list (car q) q1)
	
	)
	(cond ((not (isLL2a q)) nil) ;if not isLL2a
		  (t (mapcar #'distribute (cadr q))
		  )
	)
)

(defun undistribute-recursive (q)
	;if q is not isLpairs1st, the function returns nil
	;else, undistribute q
	(defun recurse(q1) ;recursive function used to undistribute
		(cond ((null q1) nil) ;reach the end
		  	(t (append (list (cadar q1)) (recurse (cdr q1 )))
		  	)
		)
	)
	(cond ((not (isLpairs1st-recursive q)) nil) ;not isLpairs1st
		  ((null q) nil)
		  (t (list (caar q) (recurse q)))
	)
)

(defun undistribute-iterative (q)
	;if q is not isLpairs1st, the function returns nil
	;else, undistribute q
	(cond  ((not (isLpairs1st-iterative q)) nil) ;not isLpairs1st
		   ((null q) nil);empty list
		   (t   (do ( (q1 q (cdr q1)) ;q1 is the rest of the list
		   			  (res nil (append res (list (cadar q1)))) 
		   			)
		   		    ((null q1) (list (caar q) res))
		   		)
		   )
	)
)

(defun undistribute-mapcar (q)
	;if q is not isLpairs1st, the function returns nil
	;else, undistribute q
	(cond	((not (isLpairs1st-every q)) nil) ;not isLpairs1st
		    ((null q) nil);empty list
			(t (list (caar q) (mapcar #'(lambda(q1)(cadr q1)) q))
			)
	)
)

(defun my-every (fun q)
	;apply fun to every element of q
	;stops if one returns nil
	(cond ((null q) t);reach the end without getting nil's
		  ((not(funcall fun (car q))) nil)	
		  (t (my-every fun (cdr q)))
	)
)

(defun lenLL(q)
	;returns 0 if q is not a list
	;else, returns the maximum of q's length and the length of the longest list w/in q
	(cond ((not(listp q)) 0);q is not a list, return 0
		  ((null q) 0)
		  (t (max (length q) (lenLL (car q)) (lenLL (cdr q))))
	)
)

(defun simplify-not (x)
	;simplifies not expression
	(cond ((equal '(not nil) x) t) ;not nil gets simplified to t
          ((equal '(not t) x) nil) ;not t gets simplified to nil
          ((null x) nil) ;end of list
          ((atom x) x) ;atom: return itself
          ((equal 'not (car x)) (cond ((equal '(t) (simplify-not (cdr x))) nil)
                                      ((equal '(nil) (simplify-not (cdr x)))t)
                                      (t (cons (car x) (simplify-not (cdr x))))
                                )
          )
          ((atom (car x)) (cons (car x) (simplify-not (cdr x)))) 
          (t (cons (simplify-not (car x)) (simplify-not(cdr x))));car is a list

	)
)

(defun orSim (x)
    (cond ((null (cdr x)) nil)
          ((isLL2 x) (cadr x))
          ((member t x) t)
          (t x)
    )                                                             
)
(defun andSim (x)
    (cond ((null (cdr x)) t)
          ((isLL2 x) (cadr x))
          ((member 'nil x)nil)
          (t x)
    )
)

(defun simplify (x)
    ;simplify and, or

    (cond ((equal '(not nil) x) t) ;not nil gets simplified to t
          ((equal '(not t) x) nil) ;not t gets simplified to nil
          ((null x) nil) ;end of list
          ((atom x) x) ;atom: return itself
          ((equal 'not (car x)) (cond ((equal '(t) (simplify-not (cdr x))) nil)
                                      ((equal '(nil) (simplify-not (cdr x)))t)
                                      (t (cons (car x) (simplify-not (cdr x))))
                                )
          )
          ((equal 'or (car x)) (orSim (remove 'nil x)))
          ((equal 'and (car x)) (andSim (remove 't x)))
          ((atom (car x)) (cons (car x) (simplify-not (cdr x)))) 
          (t (cons (simplify-not (car x)) (simplify-not(cdr x))));car is a list

	)
)

