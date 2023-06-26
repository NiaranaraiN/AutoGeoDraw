;;;孔序增减判断
(defun addorless(knum bullv) ;;knum孔序传入 bullv当前加减状态判断
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

;;孔序加减函数
(defun cgnumber(knum bullv)
	(setq knum
		(cond
			((= 0 bullv) (- knum 1))
			((= 1 bullv) (1+ knum))
    )
  )
)

;;灌浆孔一二三序孔块检查与生成
(defun creatcgblock(cirpt cirr) ;;cirpt:选取圆心 cirr 灌浆孔半径
  ;;制作一序孔块
  (if (= (tblsearch "block" "一序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast))
		(command "block" "一序孔xCadx" cirpt cgforb "")
		)
	)
  ;;制作三序孔块
	(if (= (tblsearch "block" "三序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast));;圆
    (command "hatch" "p" "solid" "s" cgforb "")
    (setq haforb (entlast));;填充
		(command "block" "三序孔xCadx" cirpt cgforb haforb "")
		)
	)
  ;;制作二序孔
	(if (= (tblsearch "block" "二序孔xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
    (setq pl1st (polar cirpt (* pi 1.5) cirr))
    (setq pl2nd (polar cirpt (* pi 0.5) cirr))
    (setq pl3rd (polar cirpt 0 cirr))
		(command "circle" cirpt cirr)
		(setq cgforb (entlast));;圆
    (command "pline" pl1st pl2nd "a" "s" pl3rd pl1st "" )
    (setq arforb (entlast));;二序孔的半圆
    (command "hatch" "p" "solid" "s" arforb "")
    (setq haforb (entlast));;填充
		(command "block" "二序孔xCadx" cirpt cgforb haforb "")
    (entdel arforb)
    (princ)
		)
	)
)

(defun xoryctrl(startpt endpt);;判断多段线大体方向xadd:沿X正方向画弧线取交点；xless：沿X负方向画弧线取交点。Y的类似。按象限分为四个方向。
  (setq anglev (angle startpt endpt))
	(cond 
		((or (<= anglev (* pi 0.25)) (>= anglev (* pi 1.75)) ) "xadd")
		((and (> anglev (* pi 0.25)) (< anglev (* pi 0.75)) ) "yadd")
		((and (>= anglev (* pi 0.75)) (<= anglev (* pi 1.25)) ) "xless")
		((and (> anglev (* pi 1.25)) (< anglev (* pi 1.75)) ) "yless")
  )
)

;;绘制灌浆孔函数
(defun c:HZWMGJK()
  (command "undo" "be")
  (vl-load-com)
  (princ "\n==========\n帷幕灌浆孔自动布置\nAuthor：Liusha.Li\n公众号:xCadx\n更多内容请访问：https://www.xcadx.com/\n==========")
  (setvar "cmdecho" 0)
  (setq prop (getreal "\n请输入图纸比例：<1000>"));;输入图形比例
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
  (setq cirr 0.71);;设定灌浆孔半径，以后确定是否设定为输入参数
  (setq inspro (/ 1000 prop));;插入块的比例
  (setq cgdis (* (/ 1000 prop) cgdis));;加比例后的灌浆间距
  (creatcgblock startpt cirr);;绘制一二三序孔的块，后面插入用。
  (setq knum 1 bullv 1 sumcg 0 cg1num 0 cg2num 0 cg3num 0);；设置初始孔号和增减方向；
  (setq cgldic (xoryctrl startpt endpt));;判定灌浆线走向,cgldic:curtaingrouting line direction:指灌浆线方向
  (setq cirpt startpt);;设置初始灌浆孔
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
					(setq discir (entlast));;获得圆心距辅助圆
					(setq interl (acet-geom-intersectwith cgline discir 1));;获取交点列表
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc);; 移圆心坐标
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
					(setq discir (entlast));;获得圆心距辅助圆
					(setq interl (acet-geom-intersectwith cgline discir 1));;获取交点列表
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc);; 移圆心坐标
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
					(setq discir (entlast));;获得圆心距辅助圆
					(setq interl (acet-geom-intersectwith cgline discir 1));;获取交点列表
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc);; 移圆心坐标
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
					(setq discir (entlast));;获得圆心距辅助圆
					(setq interl (acet-geom-intersectwith cgline discir 1));;获取交点列表
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc);; 移圆心坐标
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
  ;;(command "layer" "m" "temp" "")
  (setvar "osmode" f3ctrl)
  (setvar "clayer" lanow)
  (princ "\n灌浆孔布置完成！共布置灌浆孔：<")(princ sumcg)(princ ">个,")
  (princ "\n其中一序孔共<")(princ cg1num)(princ ">个；")
  (princ "\n其中二序孔共<")(princ cg2num)(princ ">个；")
  (princ "\n其中三序孔共<")(princ cg3num)(princ ">个；")(princ)
  (alert (strcat "\n灌浆孔布置完成！共布置灌浆孔：<" (vl-princ-to-string sumcg) ">个," "\n其中一序孔共<" (vl-princ-to-string cg1num) ">个；" "二序孔共<" (vl-princ-to-string cg2num) ">个；" "三序孔共<" (vl-princ-to-string cg3num) ">个；") 
  )
  (command "undo" "e")
)


