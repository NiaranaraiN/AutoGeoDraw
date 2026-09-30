                                                                         
                                                         
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                                                         

                        
(defun bakup_env()
	(setq osmode_ctrl (getvar "osmode"))              
	(setq clayer_ctrl (getvar "clayer"))              
)         

                 
(defun recover_env()
  	(setvar "osmode" osmode_ctrl)
  	(setvar "clayer" clayer_ctrl)	
)

                                         
                           
                       
                       
                       
                       
                       
                                         
(defun szd_man_program( / __section_line __section_point __is_success  __obj_section_line __at_section_line_distance __section_start_point __start_x  __section_x __section_y __section_distance __pipe_station_value __elevation_value __text_ang_0 text_ang_90 __section_max_point __section_min_point __max_y __min_y __highest_point __lowest_point __obj_temp_line __temp_line_dxf __zh_insert_point __elevation_point)
  (setq __is_success  0 __section_line nil)
  (setvar "osmode" 1573)
  (while (= __is_success 0)
		(setq __section_line (car (entsel "选择剖面线：")))
    (if (or (= (cdr (assoc 0 (entget __section_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget __section_line))) "POLYLINE") (= (cdr (assoc 0 (entget __section_line))) "LINE") )
			(progn
				(setq __obj_section_line (vlax-ename->vla-object __section_line))
				(setq __is_success 1)
      ))
	)
  (setvar "osmode" 14335)
  (setq __is_success  0 )
  (while (= __is_success 0)
		(setq __section_point (getpoint "选择剖面线上的点："))
		(setq __at_section_line_distance (vlax-curve-getdistatpoint __obj_section_line __section_point))
		                     
		(if __at_section_line_distance
				(setq __is_success  1)
		)
  )
	(setq __section_start_point (vlax-curve-getStartPoint __obj_section_line))                   
  (setq __start_x (car __section_start_point) __section_x (car __section_point) __section_y (cadr __section_point))
	(setq __section_distance (- __section_x  __start_x))
  (setq __elevation_value (rtos __section_y 2 2)  __pipe_station_value (rtos __section_distance 2 2) __text_ang_0 0 text_ang_90 1.5708 )
  
  (vla-getboundingbox __obj_section_line '__section_max_point '__section_min_point)
	(setq __section_max_point (vlax-safearray->list __section_max_point))
	(setq __section_min_point (vlax-safearray->list __section_min_point))
  (setq __max_y (+ (cadr __section_max_point) 50.0))
  (setq __min_y (- (cadr __section_max_point) 100.0))
  (setq __highest_point (list __section_x __max_y 0.0))
  (setq __lowest_point (list __section_x __min_y 0.0))
	(command "line" __lowest_point __highest_point "")
  (setq __obj_temp_line (entlast))
  (setq __temp_line_dxf (entget __obj_temp_line))
	(setq __temp_line_dxf (subst (cons 62 53) (assoc 62 __temp_line_dxf) __temp_line_dxf))
	(setq __temp_line_dxf (subst (cons 8 "Temp") (assoc 8 __temp_line_dxf) __temp_line_dxf))
	(entmod __temp_line_dxf)
  (setq __zh_insert_point (list (1- (car __lowest_point)) (1+ (cadr __lowest_point)) 0.0))
  
  (setq __elevation_point (list
		(+ 1 (car __section_point))
		(+ 1 (cadr __section_point))
		0.0
	))
	(setq __station_point_online (list
		(-  (car __section_point) 1)
		(+  (cadr __section_point) 1)
		0.0
	))
  (princ __elevation_point)
  (entmake_text __station_point_online __pipe_station_value text_ang_90)
  (entmake_text __zh_insert_point __pipe_station_value text_ang_90)
  (entmake_text __elevation_point __elevation_value __text_ang_0)
                                               
	                                                               
	                                                                
  (alert (strcat "桩号：" (rtos __section_distance 2 2) "\n高程:" (rtos __section_y 2 2) "" ))
)

(defun dszd_man_program ( / __is_success __section_line __obj_section_line __section_start_point __distance_find __get_distance_x __section_max_point __section_min_point __max_y __min_y __highest_point __lowest_point __obj_temp_line __temp_line_dxf __zh_insert_point __inter_points_list  )
  (setq __is_success  0 __section_line nil)
  (setvar "osmode" 1573)
  (while (= __is_success 0)
		(setq __section_line (car (entsel "选择剖面线：")))
    (if (or (= (cdr (assoc 0 (entget __section_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget __section_line))) "POLYLINE") (= (cdr (assoc 0 (entget __section_line))) "LINE") )
			(progn
				(setq __obj_section_line (vlax-ename->vla-object __section_line))
    		(setq __section_start_point (vlax-curve-getStartPoint __obj_section_line))                   
				(setq __is_success 1)
      ))
	)
	(setq __distance_find (getreal "查找桩号位置："))
  (setq __get_distance_x (+ __distance_find (car __section_start_point)))
	(vla-getboundingbox __obj_section_line '__section_max_point '__section_min_point)
	(setq __section_max_point (vlax-safearray->list __section_max_point))
	(setq __section_min_point (vlax-safearray->list __section_min_point))
  (setq __max_y (+ (cadr __section_max_point) 300.0))
  (setq __min_y (- (cadr __section_max_point) 300.0))
  (setq __highest_point (list __get_distance_x __max_y 0.0))
  (setq __lowest_point (list __get_distance_x __min_y 0.0))
  (command "line" __lowest_point __highest_point "")
  (setq __obj_temp_line (entlast))
  (setq __temp_line_dxf (entget __obj_temp_line))
	(setq __temp_line_dxf (subst (cons 62 53) (assoc 62 __temp_line_dxf) __temp_line_dxf))
	(setq __temp_line_dxf (subst (cons 8 "Temp") (assoc 8 __temp_line_dxf) __temp_line_dxf))
	(entmod __temp_line_dxf)
	(setq __zh_insert_point (list (1- (car __lowest_point)) (1+ (cadr __lowest_point)) 0.0))
  (setq __obj_temp_line (vlax-ename->vla-object __obj_temp_line))
  (setq __inter_points (vla-IntersectWith __obj_section_line __obj_temp_line acExtendNone))
  (setq __inter_points_list (vlax-safearray->list (vlax-variant-value __inter_points)))
  (setq __insert_points (list (1- (car __inter_points_list)) (1+ (cadr __inter_points_list)) 0.0))
	                                                                                                                        
                                                               
                              
  (entmake_text __insert_points (rtos __distance_find)  1.5708)
  (entmake_text __zh_insert_point (rtos __distance_find)  1.5708)
                                                                                        

  
)

(defun dgc_program ( / __section_point __elevation_point __elevation_value )
	(setvar "osmode" 14335)
  (setq __section_point (getpoint "选择高程点："))
	(setq __elevation_point (list
		(+ 1 (car __section_point))
		(+ 1 (cadr __section_point))
		0.0
	))
  (setq __elevation_value (rtos (cadr __section_point) 2 2))
  (entmake_text __elevation_point __elevation_value 0)
  (make-line-this-func __elevation_point)
  (princ)
)

(defun azd_program ( / __is_success __plan_line __obj_plan_line __plan_point __at_plan_line_distance __station_point_online __created_point __group_ss __plan_point_str) 
  (setq __is_success  0 __plan_line nil)
  (setvar "osmode" 1573)
  (while (= __is_success 0)
		(setq __plan_line (car (entsel "选择平面图上的剖面线：")))
    (if (or (= (cdr (assoc 0 (entget __plan_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget __plan_line))) "POLYLINE") (= (cdr (assoc 0 (entget __plan_line))) "LINE") )
			(progn
				(setq __obj_plan_line (vlax-ename->vla-object __plan_line))
				(setq __is_success 1)
      ))
	)
  (setvar "osmode" 14335)
  (setq __is_success  0 )
  (while (= __is_success 0)
		(setq __plan_point (getpoint "选择平面剖线上的点："))
		(setq __at_plan_line_distance (vlax-curve-getdistatpoint __obj_plan_line __plan_point))
		                     
		(if __at_plan_line_distance
				(setq __is_success  1)
		)
  )
	(setq __station_point_online (list
	(+  (car __plan_point) 1)
	(+  (cadr __plan_point) 1)
	0.0
	))
  (setq __group_ss (ssadd))
  (setq __plan_point_str (strcat (rtos (car __plan_point) 2 2) "," (rtos (cadr __plan_point) 2 2)))
  (command "point" __plan_point_str)
  (setq __created_point (entlast))
  (modified-temp-layer __created_point)
  (ssadd  __created_point __group_ss)
  (entmake_text __station_point_online (rtos __at_plan_line_distance 2 2) 0)
  (ssadd  (entlast) __group_ss)
  (command "_.GROUP" "c" "*" "" __group_ss "")
)


(defun dazd_program ( / __is_success __plan_line __obj_plan_line __distance_find __finded_point __station_point_online __group_ss __plan_point_str __created_point  __dist_text __zoom_obj __last_point __max_dist)
	(setq __is_success  0 __plan_line nil)
  (setvar "osmode" 1573)
  (while (= __is_success 0)
		(setq __plan_line (car (entsel "选择剖面线：")))
    (if (or (= (cdr (assoc 0 (entget __plan_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget __plan_line))) "POLYLINE") (= (cdr (assoc 0 (entget __plan_line))) "LINE") )
			(progn
				(setq __obj_plan_line (vlax-ename->vla-object __plan_line))
				(setq __is_success 1)
      ))
	)
  (setq __distance_find (getreal "查找桩号位置："))
  (setq __max_dist (vlax-curve-getDistAtPoint __obj_plan_line (vlax-curve-getEndPoint __obj_plan_line)))
  (PRINC __MAX_DIST)
  (if (> __distance_find __max_dist)
		(princ "超出剖线长度！")
		(progn
      (setq __finded_point (vlax-curve-getPointAtDist __obj_plan_line __distance_find))
			(setq __station_point_online (list
				(+  (car __finded_point) 1)
				(+  (cadr __finded_point) 1)
				0.0
			))
			(setq __group_ss (ssadd) __zoom_obj (ssadd))
			(setq __plan_point_str (strcat (rtos (car __finded_point) 2 2) "," (rtos (cadr __finded_point) 2 2)))
			(command "point" __plan_point_str)
			(setq __created_point (entlast))
			(modified-temp-layer __created_point)
			(ssadd  __created_point __group_ss)
			(entmake_text __station_point_online (rtos __distance_find 2 2) 0)
			(setq __dist_text (entlast))
			(ssadd  __dist_text __group_ss)
			(command "_.zoom" "O" __group_ss "")
			(command "_.GROUP" "C" "*" "" __group_ss "")
    )	
	)
  (PRINC)
)
  


(defun entmake_text ( _text_point _text_txt _text_angle / __text-entity __result )
    (setq __text-entity 
        (list 
          (cons 0 "TEXT")           
          (cons 100 "AcDbText")         
          (cons 10 _text_point)         
          (cons 40 2.5)           
          (cons 1 _text_txt)           
          (cons 7 "宋体2.5")           
          (cons 8 "Temp")       
          (cons 62 51)       
          (cons 50 _text_angle)           
        )
  )
  (setq __result (entmake __text-entity))               
	__result
)

(defun make-xline (self.xline_center_point self.verctor_point / __maked_xline __result)                                                                                 
	(setq __maked_xline
		(list
			'(0 . "XLINE")            
			'(100 . "AcDbEntity")
			'(100 . "AcDbXline")
			(cons 10 self.xline_center_point)   
			(cons 11 self.verctor_point)
		)
	)
    (if (not *set_line_color)
    (setq *set_line_color 7)
		(setq __maked_xline (append  __maked_xline (list (cons 62 *set_line_color))))
	)
  (if (not *set_line_layer)
		(setq *set_line_layer "Temp")
		(setq __maked_xline (append  __maked_xline (list (cons 8 *set_line_layer))))
	)
	(setq __result (entmake __maked_xline))
  __result
)

(defun make-line-this-func ( self.get_point /  __highest_point __lowest_point __obj_temp_line __temp_line_dxf)
  (setq __highest_point (list (car self.get_point) (+ (cadr self.get_point) 50.0) 0.0))
  (setq __lowest_point (list (car self.get_point) (- (cadr self.get_point) 100.0) 0.0))
	(command "line" __lowest_point __highest_point "")
  (setq __obj_temp_line (entlast))
  (setq __temp_line_dxf (entget __obj_temp_line))
	(setq __temp_line_dxf (subst (cons 62 53) (assoc 62 __temp_line_dxf) __temp_line_dxf))
	(setq __temp_line_dxf (subst (cons 8 "Temp") (assoc 8 __temp_line_dxf) __temp_line_dxf))
	(entmod __temp_line_dxf)
	(princ)
)

(defun modified-temp-layer (get_ent / __temp_line_dxf )
  (setq __temp_line_dxf (entget (entlast)))
	(setq __temp_line_dxf (subst (cons 62 53) (assoc 62 __temp_line_dxf) __temp_line_dxf))
	(setq __temp_line_dxf (subst (cons 8 "Temp") (assoc 8 __temp_line_dxf) __temp_line_dxf))
	(entmod __temp_line_dxf)
)

         
(defun c:szd( / *error* e_lst)
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
	(szd_man_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)

(defun c:dszd( / *error* e_lst)
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
	(dszd_man_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)

(defun c:dgc( / *error* e_lst)
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
	(dgc_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)

(defun c:azd( / *error* e_lst)
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
	(azd_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)

(defun c:dazd( / *error* e_lst)
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
	(dazd_program)

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)