

                                                                         
                                           
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                                                         

                        
(defun bakup_env()
	(setq osmode_ctrl (getvar "osmode"))              
	(setq clayer_ctrl (getvar "clayer"))              
)         

                 
(defun recover_env()
  	(setvar "osmode" osmode_ctrl)
  	(setvar "clayer" clayer_ctrl)	
)


(defun addorless(knum bullv)                                     
	(setq bully 
		(cond 
    	((and (= 1 knum) (= 1 bullv)) 1)
			((and (= 2 knum) (= 1 bullv)) 1)
			((and (= 3 knum) (= 1 bullv)) 0)
			((and (= 3 knum) (= 0 bullv)) 0)
			((and (= 2 knum) (= 0 bullv)) 0)
			((and (= 1 knum) (= 0 bullv)) 1)
  )
  )
 )

              
(defun cgnumber(knum bullv)
	(setq knum
		(cond
			((= 0 bullv) (- knum 1))
			((= 1 bullv) (1+ knum))
    )
  )
)

                              
(defun creatcgblock(cirpt cirr)                                 
                
  (if (= (tblsearch "block" "一序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast))
		(command "block" "一序孔xCadx" cirpt cgforb "")
		)
	)
                
	(if (= (tblsearch "block" "三序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast))    
    (command "-hatch" "p" "solid" "s" cgforb "")
    (setq haforb (entlast))      
		(command "block" "三序孔xCadx" cirpt cgforb haforb "")
		)
	)
              
	(if (= (tblsearch "block" "二序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
    (setq pl1st (polar cirpt (* pi 1.5) cirr))
    (setq pl2nd (polar cirpt (* pi 0.5) cirr))
    (setq pl3rd (polar cirpt 0 cirr))
		(command "circle" cirpt cirr)
		(setq cgforb (entlast))    
    (command "pline" pl1st pl2nd "a" "s" pl3rd pl1st "" )
    (setq arforb (entlast))              
    (command "-hatch" "p" "solid" "s" arforb "")
    (setq haforb (entlast))      
		(command "block" "二序孔xCadx" cirpt cgforb haforb "")
    (entdel arforb)
    (princ)
		)
	)
)

(defun xoryctrl(startpt endpt)                                                                                                           
  (setq anglev (angle startpt endpt))
	(cond 
		((or (<= anglev (* pi 0.25)) (>= anglev (* pi 1.75)) ) "xadd")
		((and (> anglev (* pi 0.25)) (< anglev (* pi 0.75)) ) "yadd")
		((and (>= anglev (* pi 0.75)) (<= anglev (* pi 1.25)) ) "xless")
		((and (> anglev (* pi 1.25)) (< anglev (* pi 1.75)) ) "yless")
  )
)
                                         
                           
                       
                       
                       
                       
                       
                                         
               



(defun start_program ()
	(princ "\n==========\n帷幕灌浆孔自动布置\nAuthor：Liusha.Li\n==========")
  (setq prop (getreal "\n请输入图纸比例：<1000>"))              
  (if (= prop nil) (setq prop 1000))
  (setq cgline (car (entsel "\n请选择帷幕线：")))
  (while (or (= cgline nil) (and (/= (cdr (assoc 0 (entget cgline))) "LWPOLYLINE") (/= (cdr (assoc 0 (entget cgline))) "LINE")))
  	(setq cgline (car (entsel "\n请选择帷幕线：")))
  )
  (setq startpt (getpoint "\n请选择灌浆孔起点："))
  (setq endpt (getpoint "\n请选择灌浆孔终点或帷幕线终点："))
  (setq cgdis (getreal "\n请输入灌浆孔间距：<2>"))
  (setq f3ctrl (getvar "osmode"))
  (setq lanow (getvar "clayer"))
  (if (= cgdis nil) (setq cgdis 2))
  (setq cirr 0.71)                                            
  (setq inspro (/ 1000 prop))              
  (setq cgdis (* (/ 1000 prop) cgdis))                    
  (creatcgblock startpt cirr)                                  
  (setq knum 1 bullv 1 sumcg 0 cg1num 0 cg2num 0 cg3num 0)                           
  (setq cgldic (xoryctrl startpt endpt))                                                                   
  (setq cirpt startpt)                
  (setq interl (list 1 2))
  (command "layer" "m" "Geo" "")
  (setvar "osmode" 0)
	(cond
		((= cgldic "xadd")
    	(progn
				(while (<= (car cirpt) (car endpt))
					(setq bname (cond ((= knum 1) "一序孔xCadx") ((= knum 2) "三序孔xCadx") ((= knum 3) "二序孔xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast))                  
					(setq interl (acet-geom-intersectwith cgline discir 1))              
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc)             
					(cond 
       			((= knum 1) (setq cg1num (1+ cg1num)) )
						((= knum 2) (setq cg3num (1+ cg3num)) )
						((= knum 3) (setq cg2num (1+ cg2num)) )
          )
					(setq bullv (addorless knum bullv))
					(setq knum (cgnumber knum bullv))
					(entdel discir)
					(setq sumcg (1+ sumcg))
        )
    	)
    )
		((= cgldic "xless")
    	(progn
				(while (>= (car cirpt) (car endpt))
					(setq bname (cond ((= knum 1) "一序孔xCadx") ((= knum 2) "三序孔xCadx") ((= knum 3) "二序孔xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast))                  
					(setq interl (acet-geom-intersectwith cgline discir 1))              
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc)             
					(cond 
       					((= knum 1) (setq cg1num (1+ cg1num)) )
						((= knum 2) (setq cg3num (1+ cg3num)) )
						((= knum 3) (setq cg2num (1+ cg2num)) )
                    )
					(setq bullv (addorless knum bullv))
					(setq knum (cgnumber knum bullv))
					(entdel discir)
					(setq sumcg (1+ sumcg))        
        )
    	)
    )   
		((= cgldic "yadd")
    	(progn
				(while (<= (car (cdr cirpt)) (car (cdr endpt)))
					(setq bname (cond ((= knum 1) "一序孔xCadx") ((= knum 2) "三序孔xCadx")((= knum 3) "二序孔xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast))                  
					(setq interl (acet-geom-intersectwith cgline discir 1))              
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc)             
					(cond 
       					((= knum 1) (setq cg1num (1+ cg1num)) )
						((= knum 2) (setq cg3num (1+ cg3num)) )
						((= knum 3) (setq cg2num (1+ cg2num)) )
                    )
					(setq bullv (addorless knum bullv))
					(setq knum (cgnumber knum bullv))
					(entdel discir)
					(setq sumcg (1+ sumcg))
        )
    	)
    )
		((= cgldic "yless")
    	(progn
				(while (>= (car (cdr cirpt)) (car (cdr endpt)))
					(setq bname (cond ((= knum 1) "一序孔xCadx") ((= knum 2) "三序孔xCadx")((= knum 3) "二序孔xCadx")))
					(command "insert" bname cirpt "" "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast))                  
					(setq interl (acet-geom-intersectwith cgline discir 1))              
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc)             
					(cond 
       					((= knum 1) (setq cg1num (1+ cg1num)) )
						((= knum 2) (setq cg3num (1+ cg3num)) )
						((= knum 3) (setq cg2num (1+ cg2num)) )
                    )
					(setq bullv (addorless knum bullv))
					(setq knum (cgnumber knum bullv))
					(entdel discir)
					(setq sumcg (1+ sumcg))       
        )
    	)
    )
  )
                                   
  (princ "\n灌浆孔布置完成！共布置灌浆孔：<")(princ sumcg)(princ ">个,")
  (princ "\n其中一序孔共<")(princ cg1num)(princ ">个；")
  (princ "\n其中二序孔共<")(princ cg2num)(princ ">个；")
  (princ "\n其中三序孔共<")(princ cg3num)(princ ">个；")(princ)
  (alert (strcat "\n灌浆孔布置完成！共布置灌浆孔：<" (vl-princ-to-string sumcg) ">个," "\n其中一序孔共<" (vl-princ-to-string cg1num) ">个；" "二序孔共<" (vl-princ-to-string cg2num) ">个；" "三序孔共<" (vl-princ-to-string cg3num) ">个；") 
  )
  
  
)


         
(defun c:HZWMGJKLLS( / *error* e_lst)
	(setq e_lst (mapcar (function (lambda (n) (list 'setvar n (getvar n)))) 
	'("autosnap" "osmode" "aperture" "hpspace" "hpassoc" "mirrtext" "auprec" "luprec" "dimzin" "cecolor")))
	(defun *error* (msg)
	(mapcar 'eval e_lst)
			(if (not (member
									msg
									'(nil "函数被取消" ";错误:quit / exit abort")
									)
						)
				(princ (strcat ";错误:" msg))
			)
	)
	(command "undo" "be")
	(bakup_env)
	         
	(vl-load-com)
	(start_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)
