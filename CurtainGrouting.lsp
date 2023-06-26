;;;���������ж�
(defun addorless(knum bullv) ;;knum������ bullv��ǰ�Ӽ�״̬�ж�
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

;;����Ӽ�����
(defun cgnumber(knum bullv)
	(setq knum
		(cond
			((= 0 bullv) (- knum 1))
			((= 1 bullv) (1+ knum))
    )
  )
)

;;�ཬ��һ������׿���������
(defun creatcgblock(cirpt cirr) ;;cirpt:ѡȡԲ�� cirr �ཬ�װ뾶
  ;;����һ��׿�
  (if (= (tblsearch "block" "һ���xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast))
		(command "block" "һ���xCadx" cirpt cgforb "")
		)
	)
  ;;��������׿�
	(if (= (tblsearch "block" "�����xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
		(command "circle" cirpt cirr)
		(setq cgforb (entlast));;Բ
    (command "hatch" "p" "solid" "s" cgforb "")
    (setq haforb (entlast));;���
		(command "block" "�����xCadx" cirpt cgforb haforb "")
		)
	)
  ;;���������
	(if (= (tblsearch "block" "�����xCadx") nil)
    (progn
    (command "layer" "m" "0" "")
    (setq pl1st (polar cirpt (* pi 1.5) cirr))
    (setq pl2nd (polar cirpt (* pi 0.5) cirr))
    (setq pl3rd (polar cirpt 0 cirr))
		(command "circle" cirpt cirr)
		(setq cgforb (entlast));;Բ
    (command "pline" pl1st pl2nd "a" "s" pl3rd pl1st "" )
    (setq arforb (entlast));;����׵İ�Բ
    (command "hatch" "p" "solid" "s" arforb "")
    (setq haforb (entlast));;���
		(command "block" "�����xCadx" cirpt cgforb haforb "")
    (entdel arforb)
    (princ)
		)
	)
)

(defun xoryctrl(startpt endpt);;�ж϶���ߴ��巽��xadd:��X�����򻭻���ȡ���㣻xless����X�����򻭻���ȡ���㡣Y�����ơ������޷�Ϊ�ĸ�����
  (setq anglev (angle startpt endpt))
	(cond 
		((or (<= anglev (* pi 0.25)) (>= anglev (* pi 1.75)) ) "xadd")
		((and (> anglev (* pi 0.25)) (< anglev (* pi 0.75)) ) "yadd")
		((and (>= anglev (* pi 0.75)) (<= anglev (* pi 1.25)) ) "xless")
		((and (> anglev (* pi 1.25)) (< anglev (* pi 1.75)) ) "yless")
  )
)

;;���ƹཬ�׺���
(defun c:HZWMGJK()
  (command "undo" "be")
  (vl-load-com)
  (princ "\n==========\n�Ļ�ཬ���Զ�����\nAuthor��Liusha.Li\n���ں�:xCadx\n������������ʣ�https://www.xcadx.com/\n==========")
  (setvar "cmdecho" 0)
  (setq prop (getreal "\n������ͼֽ������<1000>"));;����ͼ�α���
  (if (= prop nil) (setq prop 1000))
  (setq cgline (car (entsel "\n��ѡ���Ļ�ߣ�")))
  (while (or (= cgline nil) (and (/= (cdr (assoc 0 (entget cgline))) "LWPOLYLINE") (/= (cdr (assoc 0 (entget cgline))) "LINE")))
  	(setq cgline (car (entsel "\n��ѡ���Ļ�ߣ�")))
  )
  (setq startpt (getpoint "\n��ѡ��ཬ����㣺"))
  (setq endpt (getpoint "\n��ѡ��ཬ���յ���Ļ���յ㣺"))
  (setq cgdis (getreal "\n������ཬ�׼�ࣺ<2>"))
  (setq f3ctrl (getvar "osmode"))
  (setq lanow (getvar "clayer"))
  (if (= cgdis nil) (setq cgdis 2))
  (setq cirr 0.71);;�趨�ཬ�װ뾶���Ժ�ȷ���Ƿ��趨Ϊ�������
  (setq inspro (/ 1000 prop));;�����ı���
  (setq cgdis (* (/ 1000 prop) cgdis));;�ӱ�����Ĺཬ���
  (creatcgblock startpt cirr);;����һ������׵Ŀ飬��������á�
  (setq knum 1 bullv 1 sumcg 0 cg1num 0 cg2num 0 cg3num 0);�����ó�ʼ�׺ź���������
  (setq cgldic (xoryctrl startpt endpt));;�ж��ཬ������,cgldic:curtaingrouting line direction:ָ�ཬ�߷���
  (setq cirpt startpt);;���ó�ʼ�ཬ��
  (setq interl (list 1 2))
  (command "layer" "m" "Geo" "")
  (setvar "osmode" 0)
	(cond
		((= cgldic "xadd")
    	(progn
				(while (<= (car cirpt) (car endpt))
					(setq bname (cond ((= knum 1) "һ���xCadx") ((= knum 2) "�����xCadx") ((= knum 3) "�����xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast));;���Բ�ľศ��Բ
					(setq interl (acet-geom-intersectwith cgline discir 1));;��ȡ�����б�
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc);; ��Բ������
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
					(setq bname (cond ((= knum 1) "һ���xCadx") ((= knum 2) "�����xCadx") ((= knum 3) "�����xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast));;���Բ�ľศ��Բ
					(setq interl (acet-geom-intersectwith cgline discir 1));;��ȡ�����б�
					(setq nextc (if (> (car (nth 0 interl)) (car (nth 1 interl)) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc);; ��Բ������
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
					(setq bname (cond ((= knum 1) "һ���xCadx") ((= knum 2) "�����xCadx")((= knum 3) "�����xCadx")))
					(command "insert" bname cirpt inspro "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast));;���Բ�ľศ��Բ
					(setq interl (acet-geom-intersectwith cgline discir 1));;��ȡ�����б�
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 0 interl) (nth 1 interl) ))
					(setq cirpt nextc);; ��Բ������
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
					(setq bname (cond ((= knum 1) "һ���xCadx") ((= knum 2) "�����xCadx")((= knum 3) "�����xCadx")))
					(command "insert" bname cirpt "" "" "")
					(command "circle" cirpt cgdis)
					(setq discir (entlast));;���Բ�ľศ��Բ
					(setq interl (acet-geom-intersectwith cgline discir 1));;��ȡ�����б�
					(setq nextc (if (> (car (cdr (nth 0 interl))) (car (cdr (nth 1 interl))) ) (nth 1 interl) (nth 0 interl) ))
					(setq cirpt nextc);; ��Բ������
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
  (princ "\n�ཬ�ײ�����ɣ������ùཬ�ף�<")(princ sumcg)(princ ">��,")
  (princ "\n����һ��׹�<")(princ cg1num)(princ ">����")
  (princ "\n���ж���׹�<")(princ cg2num)(princ ">����")
  (princ "\n��������׹�<")(princ cg3num)(princ ">����")(princ)
  (alert (strcat "\n�ཬ�ײ�����ɣ������ùཬ�ף�<" (vl-princ-to-string sumcg) ">��," "\n����һ��׹�<" (vl-princ-to-string cg1num) ">����" "����׹�<" (vl-princ-to-string cg2num) ">����" "����׹�<" (vl-princ-to-string cg3num) ">����") 
  )
  (command "undo" "e")
)


