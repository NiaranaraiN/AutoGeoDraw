1`;;;;--------卧龙快速填充-----------20240924 ****by卧龙***
(defun c:H1 ()
 (setq	msg "[斜线填充(1)/素混凝土(2)/木工板基层(3)/铝板十字图(4)/铝板点图(5)/穿孔示意(6)/木纹填充(7)/拉网板(8)/钢筋混凝土(9)/玻璃幕墙(0)/金属截面(00)/石材截面(01)/实心填充(02)/大理石(03)]")
 (initget "00 01 02 03 0 1 2 3 4 5 6 7 8 9")
 (setq kw (getkword msg))
 (setq OL (getvar "clayer"));;;图层记忆
        (setq t T)
  (while t
                (if
                        (setq pt (getpoint"\n请拾取填充内部点：(***跳过则为选择填充区域填充）"))
   (cond
    ((= kw "0") (command "-hatch" "p" "玻璃影1" 500 0  PT "")(tctc));;;本人做铝单板的填充样式，需要更换为个人行业内的填充样式和比例及角度的，自行调整
    ((= kw "1") (command "-hatch" "P" "ANSI31" 20 0 pt "" )(tctc))
    ((= kw "2") (command "-hatch" "P" "AR-CONC" 1 0  pt "" )(tctc))
    ((= kw "3") (command "-hatch" "P" "CORK" 1 0  pt "" )(tctc))
    ((= kw "4") (command "-hatch" "P" "CROSS" 10  0  pt "" )(tctc))
    ((= kw "5") (command "-hatch" "P" "DOTS" 30 0 pt "" )(tctc))
    ((= kw "6") (command "-hatch" "P" "HEX" 20 0 pt "" )(tctc))
    ((= kw "7") (command "-hatch" "P" "木纹5" 100 0 pt "" )(tctc))
    ((= kw "8") (command "-hatch" "P" "菱形金属网" 100 0 pt "" )(tctc))
    ((= kw "9") (command "-hatch" "P" "钢筋混凝土" 50 0 pt "" )(tctc))
    ((= kw "00") (command "-hatch" "P"  "ANSI32" 1 0 pt "" )(tctc))
    ((= kw "01") (command "-hatch" "P" "ANSI33" 1 0 pt "" )(tctc))
    ((= kw "02") (command "-hatch" "P" "SOLID" pt "" )(tctc))
    ((= kw "03") (command "-hatch" "P" "大理石4" 1000 0 pt "" )(tctc))
)
;                        (command "-hatch" "O" "D" "L" "Y" pt "");;;不想设置填充样式就把cond内的全部删除
                        (progn
                                (princ "\n快捷填充，请选择填充区域")
                                (setq ss (ssget '((0 . "POLYLINE,LWPOLYLINE,CIRCLE,ELLIPSE"))))
                                (setq   i -1  )
                                (setq num (sslength ss))
                                (repeat num
                                        (setq entnam (ssname ss (setq i (1+ i))))
                                )
;;(command "-hatch" "p" "angle" 15 0 "Y" ss "" ""):::不想设置填充样式就把cond内的全部删除
   (cond
    ((= kw "0") (command "-hatch" "p" "玻璃影1" 500 0 "S" ss "" "")(tctc))
    ((= kw "1") (command "-hatch" "p"  "ANSI31" 20 0 "S" ss "" "")(tctc))
    ((= kw "2") (command "-hatch" "p"  "AR-CONC" 1 0 "S" ss "" "")(tctc))
    ((= kw "3") (command "-hatch" "p"  "CORK" 1 0 "S" ss "" "")(tctc))
    ((= kw "4") (command "-hatch" "p"  "CROSS" 10  0 "S" ss "" "")(tctc))
    ((= kw "5") (command "-hatch" "p"  "DOTS" 30 0 "S" ss "" "")(tctc))
    ((= kw "6") (command "-hatch" "p"  "HEX" 20 0 "S" ss "" "")(tctc))
    ((= kw "7") (command "-hatch" "p"  "木纹5" 100 0 "S" ss "" "")(tctc))
    ((= kw "8") (command "-hatch" "p"  "菱形金属网" 100 0 "S" ss "" "")(tctc))
    ((= kw "9") (command "-hatch" "p"  "钢筋混凝土" 50 0 "S" ss "" "")(tctc))
    ((= kw "00") (command "-hatch" "p"  "ANSI32" 1 0 "S" ss "" "")(tctc))
    ((= kw "01") (command "-hatch" "p"  "ANSI33" 1 0 "S" ss "" "")(tctc))
    ((= kw "02") (command "-hatch" "p"  "SOLID" "S" ss "" "")(tctc))
    ((= kw "03") (command "-hatch" "p"  "大理石4" 1000 0 "S" ss "" "")(tctc))
)
                        );;;end pro
                );;end if
(setvar "clayer" OL);;;图层还原
        );;;end wh

(princ "\n卧龙快速填充，请退出")
);;;end dun


;;;归填充层的子层序，不需要的话，直接删除(tctc)
(defun tctc()
        (command "_layer" "m" "A-填充" "c" "8" "" "")
        (command "change" "l" "" "p" "la" "A-填充" "")
)
(princ)
;;;---------------------结束快速填充