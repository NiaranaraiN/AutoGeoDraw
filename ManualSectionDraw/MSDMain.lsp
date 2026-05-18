;--加载区--
	; (alert "test 2024.09.17 20:10")
	; (alert (strcat "命令：\n" "初始设置：    InitSectionVariants\n" "重置数据：    ResetAllValue\n" "选剖面线点：AddSectionPoint\n" "选平面线点：AddPlanPoint\n"  "剖面->平面：FDSA-find-section->plan \n" "平面->剖面：FDAS-find-plan->section \n" "备份数据：    BakupSectionData\n" "恢复数据 ：    ImportSectionData\n"))
	(if (null vlax-dump-object) (vl-load-com));将Visual LISP扩展功能加载到 AutoLISP
	(load "TbxlsCore")
	(load "TbxlsIO")
  (load "TbxlsText")
	(load "TbxlsGraphElement")
	
	(setq DOC (vla-get-ActiveDocument (vlax-get-acad-object)))
	(SETQ OPEN_PATH "W:\\")
  (setq RESET_VARIANTS (list "*set_line_color"  "*plan_line"  "*plan_start_point" "*plan_end_point" "*plan_line_obj" "*plan_scale" "*section_line" "*section_start_point" "*section_end_point" "*section_line_obj" "*section_scale" "*section_points_x" "*structure_points_x" "*strata_count" "*plan_start_distance" "*plan_end_distance" "*section_start_distance" "*section_end_distance" "*x_scale" "*y_scale" "*section_start_x" "*set_line_layer" ))
; (defun vls-Modify-SystemVariant ( / modifiedVarlist) ;设置程序运行时要修改的系统变量及变量值
; ;   "设置程序运行时要修改的系统变量及变量值" 
; ;   "返回系统变量名称-值的一个列表" 
; ;   (setq modifiedVarlist
; ;     (list 
; ;       (list "DIMZIN" 0);保留前导和后续的零
; ;       (list "CMDECHO" 0) ;关闭回显
; ;       (list "CLAYER" (getvar "CLAYER")) ;修改图层
; ;       (list "OSMODE" 0) ;关闭对象捕捉
; ;    )
; ;   )
; ;   modifiedVarlist
; )
;--*--


;(vl-cmdf "xline" "v" *section_start_point "")

(defun C:SOH-section-Order-Help ( / )
  (alert (strcat "命令(SOH)：\n" "初始设置：    InitSectionVariants\n" "重置数据：    ResetAllValue\n" "选剖面线点：AddSectionPoint\n" "选平面线点：AddPlanPoint\n"  "剖面->平面：FDSA-find-section->plan \n" "平面->剖面：FDAS-find-plan->section \n" "备份数据：    BakupSectionData\n" "恢复数据 ：    ImportSectionData\n" "删除上一点 ：    Remove-last-point\n" "删除点数据 ：    RSTRemoveSectionPoint\n" ))
	
)

;这里是加载时提示 一次帮助
(C:SOH-section-Order-Help)

;--------------单点画线功能--------------------

(defun draw-strata-height-line-now (self.on_ground_point self.strata_height_list self.line_obj_list / __strata_limits_points __draw_line_points __curr_point_x __height_index __strata_height __height_list_length  __line_maked );剖面线点表加点处理
  (setq __curr_point_x (car self.on_ground_point))
  (setq __strata_height (cadr self.on_ground_point));地面Y值
  (setq __height_index  0 __height_list_length (length self.strata_height_list))
  (setq *set_line_layer "Geo_说明_不打印")
  (while (< __height_index __height_list_length)
		(setq __strata_height (- __strata_height (* (nth __height_index self.strata_height_list) *y_scale)	))
    ;将各层坐标各组成一个列表，后期根据排序进行画线
		(setq __strata_limits_points (append __strata_limits_points (list (list __curr_point_x __strata_height))))
    (if (= __height_index 0)
      	(progn
        	(setq __draw_line_points (list self.on_ground_point (nth __height_index __strata_limits_points)))
        	(setq *set_line_color (1+ __height_index))
        	(if (> (nth __height_index self.strata_height_list) 0)
          	(progn
							(points-draw-polyline __draw_line_points)
          		(setq self.line_obj_list (append self.line_obj_list (list (vlax-ename->vla-object (entlast))) ))
						)
          )
				)
      	(progn
        	(setq __draw_line_points (list (nth (1- __height_index) __strata_limits_points) (nth __height_index __strata_limits_points)))
        	(setq *set_line_color (1+ __height_index))
        	(if (> (nth __height_index self.strata_height_list) 0)
						(progn
							(points-draw-polyline __draw_line_points)
          		(setq self.line_obj_list (append self.line_obj_list (list (vlax-ename->vla-object (entlast))) ))
						)
					)
				)
		)
    
		(setq __height_index (1+ __height_index))
  )
  (vls-obj-list->groupunname self.line_obj_list)
)



; (defun draw-at-section-point ( / __strata_height_list  __get_section_point __strata_height_all __strata_height_str __strata_height_list_str ) ;直接在剖面上加点
; 	(setvar "osmode" 14335)
; 	(setq __get_section_point (getpoint "选择剖面图上的点："))
;   (setq __strata_height_all (getstring "输入岩土层厚度："))
;   (setvar "osmode" 0)
;   (setq __strata_height_list_str (vls-string->list __strata_height_all ","))
; 	(if (not *strata_count)
; 		(setq *strata_count (length __strata_height_list_str))
;   )
; 	(setq __strata_height_list nil)
;   (foreach __strata_height_str __strata_height_list_str
; 		(setq __strata_height_list (append  __strata_height_list (list (atof __strata_height_str))))
;   )
;   (draw-strata-height-line-now __get_section_point __strata_height_list)
; )

; (defun draw-at-plan-point ( / __strata_height_list __get_plan_point __strata_height_all __section_ground_point __strata_height_str);在平面上加点，计算到剖面上的点并加入点列表
;   (setvar "osmode" 14335)
; 	(setq __get_plan_point (getpoint "选择平面图上的点："))
;   (setq __strata_height_all (getstring "输入岩土层厚度："))
;   (setvar "osmode" 0)
;   (setq __strata_height_list_str (vls-string->list __strata_height_all ","))
; 	(if (not *strata_count)
; 		(setq *strata_count (length __strata_height_list_strl))
;   )
;   (setq __strata_height_list nil)
;   (foreach __strata_height_str __strata_height_list_str
; 		(setq __strata_height_list (append __strata_height_list (list (atof __strata_height_str))))
;   )
;   (setq __section_ground_point (plan-point->section-point __get_plan_point))
;   (draw-strata-height-line-now __section_ground_point __strata_height_list)   
; )


;--------------单点画线功能-END-------------------

;--------------平剖线相互寻点功能-START-------------------

(defun find-section->plan ( / __at_line __curr_point __at_plan_point __get_distance __xline_list);通过剖面上的点，获得平面图对应位置
  (setq __point_at_line nil)
  (while (not __point_at_line);选择正确的点
    (setvar "osmode" 14335)
		(setq __curr_point (getpoint "选择平面或剖面图上的点："))
		(setvar "osmode" 0)
    (setq __at_plan_line_distance (vlax-curve-getdistatpoint *plan_line_obj __curr_point))
    (if __at_plan_line_distance
      ;点在平面图剖面上：
      (progn
        (setq __at_plan_line_point __curr_point)
				(setq __at_section_line_point (plan-point->section-point __at_plan_line_point));求得剖面point值
      	(setq __point_at_line T)
      )
      ;点不在平面图上：
      (progn      
        (setq __at_section_line_distance (vlax-curve-getdistatpoint *section_line_obj __curr_point))
        ;点在剖面图地面线上：
        (if __at_section_line_distance
					(progn 
						(setq __at_section_line_point __curr_point)
						(setq __section_point_to_start_distance (- (car __at_section_line_point)  (car *section_start_point)))
						(setq __at_plan_line_point  (section-distance->plan-point __section_point_to_start_distance))
						(setq __point_at_line T)
					)
				) 
      )
    )
	)
  (progn 
		(setq *set_line_layer nil *set_line_color 20)
		(make-xline __at_plan_line_point '(0 1 0))
		(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		(make-xline __at_plan_line_point '(1 0 0))
		(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		(make-xline __at_section_line_point '(0 1 0))
		(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		(vls-obj-list->groupunname __xline_list)
	)
  ;--
		; (setq __at_line nil)
		; (while (not __at_line)
		; 	(setvar "osmode" 14335)
		; 	(setq __curr_point (getpoint "选择剖面图上的点："))
		; 	(setvar "osmode" 0)
		;   (if (and (<= (car __curr_point) (car *section_end_point)) (>= (car __curr_point)  (car *section_start_point)))
		;     (progn 
		; 			(setq __get_distance (- (car __curr_point)  (car *section_start_point)))
		; 			(setq __at_plan_point  (section-distance->plan-point __get_distance))
		;  			(setq *set_line_layer nil *set_line_color 20)
		;   		(make-xline __at_plan_point '(0 1 0))
		;       (setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		;   		(make-xline __at_plan_point '(1 0 0))
		;       (setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		;   		(make-xline __curr_point '(0 1 0))
		;       (setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
		;       (vls-obj-list->groupunname __xline_list)
		; 			(setq __at_line T)
		;     )
		; 	)

			
		; )
)

(defun find-plan->section ( / __curr_point __x_increment __section_point __xline_listz` __point_at_line);通过平面面上的点，获得剖面面图对应位置
  (setq __point_at_line nil)
  (while (not __point_at_line)
		(setvar "osmode" 14335)
		(setq __curr_point (getpoint "选择平面图上的点："))
		(setvar "osmode" 0)
		(if (setq __point_at_line (vlax-curve-getdistatpoint *plan_line_obj __curr_point))
			(progn
     		(setq __section_point (plan-point->section-point __curr_point))
				(setq *set_line_layer nil *set_line_color 20)
				(make-xline __section_point '(0 1 0))
				(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
				(make-xline __curr_point '(0 1 0))
				(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
				(make-xline __curr_point '(1 0 0))
				(setq __xline_list (append __xline_list (list (vlax-ename->vla-object (entlast)))))
				(vls-obj-list->groupunname __xline_list)
				(setq __point_at_line T)
			)
		)
	)
)

;--------------平剖线相互寻点功能-END-------------------

;--------------数据导入导出功能-START-------------------

(defun bakup-variants->file-line (self.variant_str self.bakup_file / __list_str)
	(cond 
		( (equal (type (eval (read self.variant_str))) 'INT) (write-line (strcat "INT" ":"  self.variant_str ":" (itoa (eval (read self.variant_str) ))) __bakup_file) )
		( (equal (type (eval (read self.variant_str))) 'REAL) (write-line (strcat "REAL" ":"  self.variant_str ":" (rtos (eval (read self.variant_str))  2 16) ) __bakup_file) )
		( (equal (type (eval (read self.variant_str))) 'STR) (write-line (strcat "STR" ":"  self.variant_str ":" (eval (read self.variant_str ))) __bakup_file) )
		( (equal (type (eval (read self.variant_str))) 'ENAME) (write-line (strcat "ENAME" ":"  self.variant_str ":" (cdr (assoc 5  (entget (eval (read self.variant_str )))))) __bakup_file) )
		( (equal (type (eval (read self.variant_str))) 'VLA-OBJECT)
			(progn 
     		(write-line (strcat "VLA-OBJECT" ":"  self.variant_str ":" (vla-get-handle (eval (read self.variant_str))) ) __bakup_file)
      )
		)
		( (equal (type (eval (read self.variant_str))) 'LIST)
			(progn 
				(foreach __list_item (eval (read self.variant_str))
					(if (not __list_str)
							(setq __list_str  (rtos __list_item 2 16))
							(setq __list_str (strcat __list_str "," (rtos __list_item 2 16)))
					))
    		(write-line (strcat "LIST" ":"  self.variant_str ":" __list_str) __bakup_file)
      )
		)
  )
)

(defun import-variants<-file-line (self.raw_data / __get_variants __var_type __var_name __var_data __recovery_list __get_list_str __list_item)
	(setq __get_variants (vls-string->list self.raw_data ":"))
	(setq __var_type (car __get_variants) __var_name (cadr __get_variants) __var_data (caddr __get_variants))
	(cond 
		((= __var_type "INT") (set (read __var_name) (atoi __var_data)) )
		((= __var_type "REAL") (set (read __var_name) (atof __var_data) ))
		((= __var_type "STR") (set (read __var_name) __var_data))
		((= __var_type "ENAME") (set (read __var_name) (handent __var_data)) )
		((= __var_type "VLA-OBJECT") (set (read __var_name) (vlax-ename->vla-object (handent __var_data))) )
		((= __var_type "LIST")
     (progn 
      (setq __get_list_str (vls-string->list __var_data ","))
      (foreach __list_item __get_list_str
				(setq __recovery_list  (append __recovery_list (list (atof __list_item))))
      )
     	(set (read __var_name) __recovery_list)
     ))
	)
)


(defun import-section-data ( / __split_path __path_index __recovery_file __data_index __temp_file __temp_data __temp_point_str );恢复数据 
  (reset-all-value)
  (alert "【导入恢复】数据")
	(setq BAK_PATH (getfiled "导入剖面数据" OPEN_PATH "vlsdata" 16))
  (setq __split_path (vls-string->list BAK_PATH "\\"))
  (setq ONLY_PATH nil __path_index 0)
  (while (< __path_index (1- (length __split_path)))
    (if (not ONLY_PATH)
			(setq ONLY_PATH (nth __path_index __split_path) )
    	(SETQ ONLY_PATH (strcat ONLY_PATH "\\" (nth __path_index __split_path)))
		)
    (setq __path_index (1+	__path_index))
  ) 
  (setq __recovery_file (open BAK_PATH "r"))
  (while  (setq __line_data (read-line __recovery_file))
					(import-variants<-file-line __line_data)
	) 
  (setq __data_index 0 )
  (while (< __data_index *strata_count)
  	(setq __temp_file (open (strcat ONLY_PATH "\\strata_points_" (itoa __data_index) ".vlsdata")  "r"))
    (while (setq __temp_data (read-line __temp_file))
    	(setq __temp_point_str (vls-string->list __temp_data ","))
    	(setq __temp_point (list (atof (car __temp_point_str)) (atof (cadr __temp_point_str)) (atof (caddr __temp_point_str)) ))
      (set (read (strcat "strata_points_" (itoa __data_index))) (append (eval (read (strcat "strata_points_" (itoa __data_index)))) (list __temp_point)))
  	)
    (close __temp_file)
    (setq __data_index (1+ __data_index))
	)  
	(setq __temp_file (open (strcat ONLY_PATH "\\ground_points" ".vlsdata")  "r"))
	(while (setq __temp_data (read-line __temp_file))
		(setq __temp_point_str (vls-string->list __temp_data ","))
		(setq __temp_point (list (atof (car __temp_point_str)) (atof (cadr __temp_point_str)) (atof (caddr __temp_point_str)) ))
		(setq *ground_points  (append *ground_points (list __temp_point)))
	)
  (close __temp_file)
  (setq OPEN_PATH BAK_PATH)
  (alert "恢复成功")
)

(defun bakup-section-data ( / __split_path __path_index __bakup_var __bakup_file __data_index __temp_file __point_data);备份数据  ;__split_path __path_index __bakup_var __bakup_file
	(alert "【备份】数据")
	(setq BAK_PATH (getfiled "备份剖面数据" OPEN_PATH "vlsdata" 33))
  (setq __split_path (vls-string->list BAK_PATH "\\"))
  (setq ONLY_PATH nil __path_index 0)
  (while (< __path_index (1- (length __split_path)))
    (if (not ONLY_PATH)
			(setq ONLY_PATH (nth __path_index __split_path) )
    	(SETQ ONLY_PATH (strcat ONLY_PATH "\\" (nth __path_index __split_path)))
		)
    (setq __path_index (1+	__path_index))
  )
  
  (setq __bakup_file (open BAK_PATH "w"))
  (foreach __bakup_var RESET_VARIANTS
    (bakup-variants->file-line __bakup_var __bakup_file)
	)
  (close __bakup_file)
  
  
  (setq __data_index 0 )
  (while (< __data_index *strata_count)
  	(setq __temp_file (open (strcat ONLY_PATH "\\strata_points_" (itoa __data_index) ".vlsdata")  "w"))
    (foreach point_data (eval (read (strcat "strata_points_" (itoa __data_index))))
			(write-line (strcat (rtos (car point_data) 2 16) "," (rtos (cadr point_data) 2 16) "," (rtos (caddr point_data) 2 16)) __temp_file)
    )
    (close __temp_file)
		(setq __data_index  (1+ __data_index))
	)
  
  
  (setq __temp_file (open (strcat ONLY_PATH "\\ground_points" ".vlsdata")  "w"))
    (foreach point_data *ground_points
			(write-line (strcat (rtos (car point_data) 2 16) "," (rtos (cadr point_data) 2 16) "," (rtos (caddr point_data) 2 16)) __temp_file)
    )
    (close __temp_file)
  (setq OPEN_PATH BAK_PATH)
  (alert "备份完成")
)

;--------------数据导入导出功能-END-------------------




(defun vls-obj-list->groupunname (self.obj_list / __curr_group_dict __new_group );将图元对象列表{self.obj_list}里的图元对象编为一组。
	(vla-AppendItems
		(vla-add (vla-get-Groups (vla-get-activedocument (vlax-get-acad-object))) "*")
		(vlax-safearray-fill (vlax-make-safearray vlax-vbObject (cons 0 (1- (length self.obj_list)))) self.obj_list )
	)
)

(defun vls-obj-list->named-group (self.obj_list self.group_name / __curr_group_dict __new_group );将图元对象列表{self.obj_list}里的图元对象编为一组。
	(vla-AppendItems
		(vla-add (vla-get-Groups (vla-get-activedocument (vlax-get-acad-object))) self.group_name)
		(vlax-safearray-fill (vlax-make-safearray vlax-vbObject (cons 0 (1- (length self.obj_list)))) self.obj_list )
	)
)

(defun vls-string->list (recv_string split_char / POS); 字符串转换为列表
	(if (setq POS (VL-STRING-SEARCH split_char recv_string))
		(PROGN (CONS (SUBSTR recv_string 1 POS) (vls-string->list (substr recv_string (+ POS 1 (STRLEN split_char))) split_char)))
		(PROGN (LIST recv_string))
	)	
)

(defun line-intersectionwith (self.line_obj1 self.line_obj2 / __result);求两个布线的交点。并返回交点坐标值（本程序中帷幕灌浆与各界限交点仅有0~1个值，所以不考虑多个交点的问题。" "返回交点坐标，列表型";self.inter_result result
  (setq self.inter_result (vlax-variant-value (vlax-invoke-method self.line_obj1 'IntersectWith self.line_obj2 0)))
  (setq __result (vlax-safearray->list self.inter_result))
  __result
)

(defun reset-all-value ( / one_var __list_index __list_length)

  (setq RESET_POINTS_LIST (LIST "*ground_points" ))
	(setq __list_index 0)
	(while (< __list_index *strata_count);岩土层厚度
		(set (read (strcat "strata_points_" (itoa __list_index))) nil)
		(set (read (strcat "strata_points_increment_" (itoa __list_index))) nil)
		(setq __list_index (1+ __list_index))
	)
  (setq *strata_lines_handle nil)
  (foreach __one_var  RESET_VARIANTS
		(set (read __one_var) nil)
  )
  (foreach __one_var  RESET_POINTS_LIST
		(set (read __one_var) nil)
  )
  (alert "初始化成功")
  ; (select-section-lines)
)



(defun section-distance->plan-point (self.section_point_x_distance / __result __plan_distance );通过剖面的X差值（距离）得到平面图上对应点的位置
  (setq __plan_distance  (abs (- *plan_start_distance (/ self.section_point_x_distance *x_scale) )))
  (setq __result (vlax-curve-getPointAtDist *plan_line_obj __plan_distance))
  __result
)

(defun plan-distance->section-distance (self.point / __result);取得平面图上选择起点与实际起点的距离（绝对值），返回加了系数的值
  (setq __result (abs (- *plan_start_distance (vlax-curve-getdistatpoint *plan_line_obj self.point) )))
  (setq __result  (* __result *x_scale))
  __result
)



(defun plan-point->section-point (self.plan_point / __section_distance __ground_x __xline_for_inter __xline_for_inter_obj __inter_point);将平面图上的坐标投影到剖面图上
  (setq __section_distance (plan-distance->section-distance self.plan_point));求得地面线上距X坐标的距离 
  (setq __ground_x (+ *section_start_x __section_distance))
  (make-xline (list __ground_x (cadr *section_start_point) 0) '(0 1 0))
  (setq __xline_for_inter  (entlast))
  (setq __xline_for_inter_obj (vlax-ename->vla-object __xline_for_inter))
  (setq __inter_point (line-intersectionwith *section_line_obj __xline_for_inter_obj))
  (entdel __xline_for_inter)
  __inter_point
	
)

(defun get-remove-element-index (self.recv_list self.remove_element / __remove_index __index_list __find_index __result __one_element);取得{self.recv_list}表内元素{self.remove_element}值的存在的第一个索引 ，没有返回nil
  (setq __remove_index 0 __index_list nil __find_index nil __result nil)
  (while (and (not __find_index) (< __remove_index (length self.recv_list)))
		(setq __one_element (nth __remove_index self.recv_list))
    (if (equal __one_element self.remove_element)
			(progn 
				(setq __result __remove_index)
				(setq __find_index T)
			)
			(setq __remove_index (1+ __remove_index))
		)
  )
  __result
)


(defun c:remove-last-point ( / __strata_index __ss_index  __ename)
	(setq *section_points_x (cdr *section_points_x))
	(setq *ground_points (cdr *ground_points))
  (setq __strata_index  0)
  (while (< __strata_index *strata_count)
    (set (read (strcat "strata_points_" (itoa __strata_index)))
     		 (cdr (eval (read (strcat "strata_points_" (itoa __strata_index))))))
  	(setq __strata_index (1+ __strata_index)) 
  )
    ;实时生成剖面线
  (if (>= (length *section_points_x) 2)
    (progn 
			(if (> (length *strata_lines_handle) 0)
				(progn
				(setq __ss_index  0)
					(while (< __ss_index (length *strata_lines_handle))
						(setq __ename (handent (nth __ss_index *strata_lines_handle)) )
						(entdel __ename)
						(setq __ss_index (1+ __ss_index))
					)
				)
			)
			(draw-all-section-line)
    )
	)
)

(defun remove-section-point ( / __point_at_line __curr_point __at_plan_line_distance  __at_plan_line_point __at_section_line_point __at_section_line_distance __section_point_to_start_distance __ss_index __ename )
	(setq __point_at_line nil)
	(while (not __point_at_line);选择要删除的剖面上的地表点{__at_section_line_point}
		(setvar "osmode" 14335)
		(setq __curr_point (getpoint "选择要删除的点："))
		(setvar "osmode" 0)
		(setq __at_plan_line_distance (vlax-curve-getdistatpoint *plan_line_obj __curr_point))
		(if __at_plan_line_distance
			;点在平面图剖面上：
			(progn
				(setq __at_plan_line_point __curr_point)
				(setq __at_section_line_point (plan-point->section-point __at_plan_line_point));求得剖面point值
				(setq __point_at_line T)
			)
			;点不在平面图上：
			(progn      
				(setq __at_section_line_distance (vlax-curve-getdistatpoint *section_line_obj __curr_point))
				;点在剖面图地面线上：
				(if __at_section_line_distance
					(progn 
						(setq __at_section_line_point __curr_point)
						(setq __point_at_line T)
					)
				) 
			)
		)
	)
  (setq __remove_index (get-remove-element-index *ground_points __at_section_line_point))
  (if (not __remove_index)
		(alert "初始未找到，删除完成！")
		(progn
			(setq __remove_point (nth __remove_index *ground_points))
			(setq *ground_points (vl-remove __remove_point *ground_points))
			(setq *section_points_x (vl-remove (car __remove_point) *section_points_x))
			(setq __strata_index  0)
			(while (< __strata_index *strata_count)
				(setq __remove_point (nth __remove_index (eval (read (strcat "strata_points_" (itoa __strata_index))))) )
				(set
          (read (strcat "strata_points_" (itoa __strata_index)))
					(vl-remove __remove_point (eval (read (strcat "strata_points_" (itoa __strata_index)))))
        )
				(setq __strata_index (1+ __strata_index))
			)
    	(alert "已找到，删除完成！")
    )
	)
	  ;实时生成剖面线
  (if (>= (length *section_points_x) 2)
    (progn 
			(if (> (length *strata_lines_handle) 0)
				(progn
				(setq __ss_index  0)
					(while (< __ss_index (length *strata_lines_handle))
						(setq __ename (handent (nth __ss_index *strata_lines_handle)) )
						(entdel __ename)
						(setq __ss_index (1+ __ss_index))
					)
				)
			)
			(draw-all-section-line)
    )
	)
  
  
  
)

(defun append-in-section-data (self.on_ground_point __strata_height_list / __curr_point_x __height_index __strata_height __height_list_length );剖面线点表加点处理
  (setq *ground_points (append (list self.on_ground_point) *ground_points))
  (setq __curr_point_x (car self.on_ground_point))
	(setq *section_points_x (append (list __curr_point_x) *section_points_x ))
  (setq __strata_height (cadr self.on_ground_point));地面Y值
  (setq __height_index  0 __height_list_length (length __strata_height_list))
  (while (< __height_index __height_list_length)
  ; (repeat __repeat_number
		(setq __strata_height (- __strata_height (* (nth __height_index __strata_height_list) *y_scale)	))
    ;将各层坐标各组成一个列表，后期根据排序进行画线
    (set (read (strcat "strata_points_" (itoa __height_index))) (append (list (list __curr_point_x __strata_height 0.0)) (eval (read (strcat "strata_points_" (itoa __height_index))))))
		(setq __height_index (1+ __height_index))
  )
)


(defun add-section-point ( / __point_at_line __strata_height_list  __curr_point __strata_height_all __strata_height_str __correct_imput __correct_input __strata_height_list_str __section_point_to_start_distance __at_plan_line_distance __at_plan_line_point __at_section_line_point __at_section_line_distance __plan_point_maked_obj __group_objects __ss_index __ename  );直接在剖面上加点
  (setq __point_at_line nil)
  (while (not __point_at_line);选择正确的点
    (setvar "osmode" 14335)
		(setq __curr_point (getpoint "选择平面或剖面图上的点："))
		(setvar "osmode" 0)
    (setq __at_plan_line_distance (vlax-curve-getdistatpoint *plan_line_obj __curr_point))
    (if __at_plan_line_distance
      ;点在平面图剖面上：
      (progn
        (setq __at_plan_line_point __curr_point)
				(setq __at_section_line_point (plan-point->section-point __at_plan_line_point));求得剖面point值
      	(setq __point_at_line T)
      )
      ;点不在平面图上：
      (progn      
        (setq __at_section_line_distance (vlax-curve-getdistatpoint *section_line_obj __curr_point))
        ;点在剖面图地面线上：
        (if __at_section_line_distance
					(progn 
						(setq __at_section_line_point __curr_point)
						(setq __section_point_to_start_distance (- (car __at_section_line_point)  (car *section_start_point)))
						(setq __at_plan_line_point  (section-distance->plan-point __section_point_to_start_distance))
						(setq __point_at_line T)
					)
				) 
      )
    )
	)
  (setq __correct_input  nil)
  (while (not __correct_input) 
  	(setq __strata_height_all (getstring (strcat "输入" (itoa *strata_count) "层岩土层厚度：")))
  	(setq __strata_height_list_str (vls-string->list __strata_height_all ","))
    (setq __correct_input (equal (length __strata_height_list_str) *strata_count))
	)
	(setq __strata_height_list nil)
  (foreach __strata_height_str __strata_height_list_str
		(setq __strata_height_list (append  __strata_height_list (list (atof __strata_height_str))))
  )
  (append-in-section-data __at_section_line_point __strata_height_list)
  (entmake (list
            	(cons 0 "POINT")
            	(cons 10 __at_plan_line_point)
							'(8 . "Temp")
							'(62 . 20)
           )	
  )
  (setq __plan_point_maked_obj (vlax-ename->vla-object (entlast)))
  (setq __group_objects (append __group_objects (list __plan_point_maked_obj)))
  (draw-strata-height-line-now __at_section_line_point __strata_height_list __group_objects)

  
  ;实时生成剖面线
  (if (>= (length *section_points_x) 2)
    (progn 
			(if (> (length *strata_lines_handle) 0)
				(progn
				(setq __ss_index  0)
					(while (< __ss_index (length *strata_lines_handle))
						(setq __ename (handent (nth __ss_index *strata_lines_handle)) )
						(entdel __ename)
						(setq __ss_index (1+ __ss_index))
					)
				)
			)
			(draw-all-section-line)
    )
	)
  (alert "添加成功")
)

(defun add-plan-point ( /);在平面上加点，计算到剖面上的点并加入点列表
  (add-section-point)
  ; (setvar "osmode" 14335)
  ; (setq __point_at_line nil)
  ; (while (not __point_at_line)
	; 	(setq __curr_point (getpoint "选择平面图上的点："))
	; 	(setq __point_at_line (vlax-curve-getdistatpoint *plan_line_obj __curr_point))
  ; )
  ; (setvar "osmode" 0)
  ; (setq __correct_input  nil)
  ; (while (not __correct_input) 
	; 	(setq __strata_height_all (getstring (strcat "输入" (itoa *strata_count) "层岩土层厚度：")))
	; 	(setq __strata_height_list_str (vls-string->list __strata_height_all ","))
  ;   (setq __correct_input (equal (length __strata_height_list_str) *strata_count))
	; )
  ; (setq __strata_height_list nil)
  ; (foreach __strata_height_str __strata_height_list_str
	; 	(setq __strata_height_list (append __strata_height_list (list (atof __strata_height_str))))
  ; )
  ; (setq __curr_ground_point (plan-point->section-point __curr_point))
  ; (append-in-section-data __curr_ground_point __strata_height_list)
  ; (draw-strata-height-line-now __curr_ground_point __strata_height_list)
  ; (alert "添加成功")
)

(defun add-structure-point ( / );加结构点
  (setvar "osmode" 14335)
	(setq *structure_point (getpoint "选择地质特征点："))
  (setq *structure_name (getstring "输入地质点描述"))
  ;(setq *structure_points_x (append *structure_points_x (list (car *structure_point))))
  (setq *structure_distance (plan-distance->section-distance *structure_point))
  (setq *structure_name_list     (append (list *structure_name) *structure_name_list ))
  (setq *structure_distance_list (append (list *structure_distance) *structure_distance_list ))
  (setvar "osmode 0")
)

(defun select-section-lines ( / __is_success );选择剖面线
  ;*plan_line,*section_line
  (setq *strata_count nil)
  (while (not *strata_count) 
  	(setq *strata_count (getint "输入岩土层层数："))
	)
  (setq __is_success  0 *plan_line nil)
  (while (= __is_success 0)
    (setvar "osmode" 1573)
		(setq *plan_line (car (entsel "选择平面图上的剖面线：")))
    (if (or (= (cdr (assoc 0 (entget *plan_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget *plan_line))) "POLYLINE") (= (cdr (assoc 0 (entget *plan_line))) "LINE") )
			(progn
				(setq *plan_line_obj (vlax-ename->vla-object *plan_line));线的对象值
				(setq __is_success 1)
      ))
    (setvar "osmode" 0)
	)
  (setq __is_success 0)
	(while (= __is_success 0)
		(setvar "osmode" 1573)
		(setq *plan_start_point (getpoint "选择起点坐标："))
		(setq *plan_end_point (getpoint "选择终点坐标："))
		(setq *plan_start_distance (vlax-curve-getdistatpoint *plan_line_obj *plan_start_point) );获得平面图起点与平面剖面线的距离
		(setq *plan_end_distance (vlax-curve-getdistatpoint *plan_line_obj *plan_end_point) );获得平面图起点与平面剖面线的距离
		(if (not (or (not *plan_start_distance) (not *plan_end_distance)))
			(progn
				(setq *plan_scale 1000.0)
				(setq *plan_scale (getreal "平面图比例1：<1000>"))
				(if (not *plan_scale) (setq *plan_scale 1000.0))
				(setq __is_success 1)
			)
		)
   	(setvar "osmode" 0)
   ) 
  (setq __is_success  0 *section_line nil)
  (while (= __is_success 0)
		(setvar "osmode" 14335)
		(setq *section_line (car (entsel "选择剖面图上的剖面线：")))
    (if (or (= (cdr (assoc 0 (entget *section_line))) "LWPOLYLINE") (= (cdr (assoc 0 (entget *section_line))) "POLYLINE") (= (cdr (assoc 0 (entget *section_line))) "LINE"))
				(setq __is_success 1)
    )
		(setvar "osmode" 0)
	)
	(setq *section_line_obj (vlax-ename->vla-object *section_line));线的对象值
  (setq __is_success  0 )
	(while (= __is_success 0)
		(setvar "osmode" 14335)
		(setq *section_start_point (getpoint "选择起点坐标："))
		(setq *section_end_point (getpoint "选择终点坐标："))
		(setq *section_start_distance (vlax-curve-getdistatpoint *section_line_obj *section_start_point) );获得平面图起点与平面剖面线的距离
		(setq *section_end_distance (vlax-curve-getdistatpoint *section_line_obj *section_end_point) );获得平面图起点与平面剖面线的距离
		(if (not (or (not *section_start_distance) (not *section_end_distance)))
      (progn
        (setq *section_scale (getreal "剖面图比例1：<1000>"))
        (if (not *section_scale) (setq *section_scale 1000.0))
				(setq __is_success 1)

			)
    )
		(setvar "osmode" 0)
	)
  (setq *x_scale (/ *plan_scale *section_scale));计算X和Y的比例
  (setq *y_scale (/ 1000.0 *section_scale))
  (setq *section_start_x (car *section_start_point));剖面起点

)


(defun strata-points-Interpolation ( / __list_index __delta_point);岩土层列表采用插值法进行扩充
	(defun insert-increment-points ( self.current_point self.next_point self.ground_start_point selt.ground_end_point / __point_index __Interpolation_points  __x_increment __y_increment __delta_x __delta_y __count_delta __start_xline __start_xline_obj __end_xline __end_xline_obj __start_difference_y __end_difference_y __difference_y_increment __delta_point_x __delta_xline_point __inter_xline __inter_xline_obj __inter_groud_point );插值 法插两个选取点之间的值,返回[{self.current_point},{self.next_point})的一个列表
    (setq  __start_difference_y (- (cadr self.ground_start_point) (cadr self.current_point)));self.current_point点距地表的初始差值
    (setq __end_difference_y (- (cadr selt.ground_end_point) (cadr self.next_point)))
    (setq __difference_y_increment (- __end_difference_y __start_difference_y) )
    (setq __x_increment (- (car self.next_point) (car self.current_point)) __y_increment (- __end_difference_y __start_difference_y));X/Y的最大增值
		(cond
			((>= __x_increment 2000) (setq __delta_x 50.0))
			((>= __x_increment 1000) (setq __delta_x 30.0))
			((>= __x_increment 500 ) (setq __delta_x 20.0))
			((>= __x_increment 100 ) (setq __delta_x 10.0))
			((>= __x_increment 10  ) (setq __delta_x 2.0 ))
			((>= __x_increment 5   ) (setq __delta_x 0.5 ))
			((> __x_increment 0    ) (setq __delta_x 0.25))
    )
    (setq __delta_y (/ __y_increment (/ __x_increment __delta_x)) __count_delta 0 )
    (while (< (+ (car self.current_point) (* __delta_x  __count_delta)) (car self.next_point) )
      (setq __delta_point_x (+ (car self.current_point) (* __delta_x  __count_delta)))
      (setq __delta_xline_point (list __delta_point_x  (cadr self.current_point) 0))
      (make-xline __delta_xline_point '(0 1 0))
      (setq __inter_xline (entlast))
      (setq __inter_xline_obj (vlax-ename->vla-object __inter_xline))
      (setq __inter_groud_point (line-intersectionwith *section_line_obj __inter_xline_obj))
      (setq __Interpolation_points	
				(append __Interpolation_points 
              	(list 
                  (list 
                    (car __inter_groud_point);X
                    (- (- (cadr __inter_groud_point) __start_difference_y ) (* __delta_y __count_delta))
										0.0
                	));y
			))
      (setq __count_delta (1+ __count_delta))
      (entdel __inter_xline)
    )
    ; (entdel __start_xline)
    ; (entdel __end_xline)
    __Interpolation_points
  )  
  (defun get-points-Interpolation-list ( self.points / __sorted_index __current_point_index __next_point_index __current_point  __result __next_ground_point __current_ground_point);对每层土列表进行扩列，返回新列表{__result}
		(setq __sorted_index 0);取原来坐标的序号
		(while (< __sorted_index (length *list_sorted_index))
			(setq __current_point_index (nth __sorted_index *list_sorted_index));获得到从小到大的坐标的序号
			(if (>= (1+ __sorted_index) (length *list_sorted_index))
				(setq __result (append __result (list (nth __current_point_index self.points))))
				(progn
					(setq __next_point_index (nth (1+ __sorted_index) *list_sorted_index));下一点的坐标序号
					(setq __current_point (nth __current_point_index self.points))
					(setq __next_point (nth __next_point_index self.points))
					(setq __current_ground_point (nth __current_point_index *ground_points))
					(setq __next_ground_point (nth __next_point_index *ground_points))
					(setq __result (append __result (insert-increment-points __current_point __next_point __current_ground_point __next_ground_point)))
        )
			)
		(setq __sorted_index (1+ __sorted_index))
		)
    __result
	)
  
  (setq __list_index 0)
	(while (< __list_index *strata_count);岩土层厚度
		(set (read (strcat "strata_points_increment_" (itoa __list_index))) (get-points-Interpolation-list (eval (read (strcat "strata_points_" (itoa __list_index))))))
		(setq __list_index (1+ __list_index))
	)
	
)

(defun points-draw-polyline (self.strata_points / __strata_polyline __cons_point __one_point __sorted_index __strata_point __cons_points __result ) 
  (setq __strata_polyline
		(list
			'(0 . "LWPOLYLINE")            ; 实体类型是轻量多段线
			'(100 . "AcDbEntity")
			'(100 . "AcDbPolyline")
			(cons 90 (length self.strata_points))   ; 顶点数量
			'(70 . 0)                      ; 闭合多段线标识
		)
	)
  (if (not *set_line_color)
    (setq *set_line_color 7)
		(setq __strata_polyline (append  __strata_polyline (list (cons 62 *set_line_color))))
	)
  (if (not *set_line_layer)
		(setq *set_line_layer "Temp")
		(setq __strata_polyline (append  __strata_polyline (list (cons 8 *set_line_layer))))
	)
  (setq __cons_points (mapcar '(lambda (__one_point) (cons 10 __one_point)) self.strata_points))
  (foreach __strata_point __cons_points
    (setq __strata_polyline (append __strata_polyline (list __strata_point)))
	)
  (setq __result (entmake __strata_polyline))
  __result
)

(defun make-xline (self.xline_center_point self.verctor_point / __maked_xline __result);输入一个点坐标{self.xline_center_point}和向量坐标{self.verctor_point}以构造XLINE
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

;-----------数据保存与恢复子函数-START--------------

(defun c:test ( / __list_index __data_index)
	(setq BAK_PATH (getfiled "备份剖面数据" "W:\\" "vlsdata" 16))
  (setq split_path (vls-string->list BAK_PATH "\\"))
  (setq ONLY_PATH nil path_index 0)
  (while (< path_index (1- (length split_path)))
    (if (not ONLY_PATH)
			(setq ONLY_PATH (nth path_index split_path) )
    	(SETQ ONLY_PATH (strcat ONLY_PATH "\\" (nth path_index split_path)))
		)
    (setq path_index (1+	path_index))
  )
  (princ ONLY_PATH)
  (setq __bakup_file (open BAK_PATH "w"))
  (write-line "STR:test_var_str:string" __bakup_file)
  (write-line "REAL:test_var_real:3.14159" __bakup_file)
  (write-line "INT:test_var_int:3" __bakup_file)
  (progn
    ; (setq list_var_list1 '((123456789.01234567890123456 123456789.01234567890123456) (987654321.987654321987654321 987654321.987654321987654321)))
  	(write-line "LIST:test_var_list:((1 2) (3 4))" __bakup_file)
	)
  (write-line "ENAME:test_var_ename:2CA2" __bakup_file)
  (CLOSE __bakup_file)
  
  (setq __data_index 0 )
  (while (< __data_index *strata_count)
  	(setq __temp_file (open (strcat ONLY_PATH "\\strata_points_" (itoa __data_index) ".vlsdata")  "w"))
    (foreach point_data (eval (read (strcat "strata_points_" (itoa __data_index))))
			(write-line (strcat (rtos (car point_data) 2 16) "," (rtos (cadr point_data) 2 16) "," (rtos (caddr point_data) 2 16)) __temp_file)
    )
    (close __temp_file)
		(setq __data_index  (1+ __data_index))
	)
  
  ;------IMPORT-----------
  (setq import_file (open BAK_PATH "r"))
  (while (setq __line_data (read-line import_file))
    (setq get_variants (vls-string->list __line_data ":"))
    (setq var_type (car get_variants) var_name (cadr get_variants) var_data (caddr get_variants))
		(cond 
			((= var_type "STR") (set (read var_name) var_data))
			((= var_type "REAL") (set (read var_name) (atof var_data) ))
			((= var_type "INT") (set (read var_name) (atoi var_data)))
			((= var_type "LIST") (set (read var_name) (read (strcat (eval var_data)))))
			((= var_type "ENAME") (set (read var_name) (handent var_data)))
    )
  )
  (close import_file)
  
  (setq __data_index 0 )
  (while (< __data_index *strata_count)
  	(setq __temp_file (open (strcat ONLY_PATH "\\strata_points_" (itoa __data_index) ".vlsdata")  "r"))
    (while (setq __temp_data (read-line __temp_file))
    	(setq __temp_point_str (vls-string->list __temp_data ","))
    	(setq __temp_point (list (atof (car __temp_point_str)) (atof (cadr __temp_point_str)) (atof (caddr __temp_point_str)) ))
      (set (read (strcat "import_strata_points_" (itoa __data_index))) (append (eval (read (strcat "import_strata_points_" (itoa __data_index)))) (list __temp_point)))
  	)
    (close __temp_file)
    (setq __data_index (1+ __data_index))
    )
    
)


(defun draw-all-section-line ( / __list_index  __last_line_obj __strata_lines_ohj)
  (if (< (length *section_points_x) 2)
		(progn (alert "剖面点过少，至少需要两点！") (exit) )
  )
  (setq __list_index 0)
  (setq *list_sorted_index (vl-sort-i *section_points_x <))
	(while (< __list_index *strata_count)
    (set (read (strcat "strata_points_increment_" (itoa __list_index))) nil)
  	(setq __list_index (1+ __list_index))
	)
  (strata-points-Interpolation)
  (setq __list_index 0 *set_line_layer "Geo_综合" *set_line_color 7 *strata_lines_handle '())

  (while (< __list_index *strata_count)
  	(points-draw-polyline (eval (read (strcat "strata_points_increment_" (itoa __list_index)))))
		(setq  *strata_lines_handle (append *strata_lines_handle (list (cdr (assoc 5 (entget (entlast)))))))
  	(setq __list_index (1+ __list_index))
	)
)
;-----------END----------------



(defun C:DrawAllSectionLine (/ *error* *CurDoc* *Old_SysVar*) ;通过平面面上的点，获得剖面面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (draw-all-section-line)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)


;命令程序
(defun C:AddPlanPoint (/ *error* *CurDoc* *Old_SysVar*) 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (add-plan-point)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:AddSectionPoint (/ *error* *CurDoc* *Old_SysVar*) 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (add-section-point)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:InitSectionVariants (/ *error* *CurDoc* *Old_SysVar*) 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (select-section-lines)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:AddStructurePoint (/ *error* *CurDoc* *Old_SysVar*) 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (add-structure-point)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:ResetAllValue (/ *error* *CurDoc* *Old_SysVar*) 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (reset-all-value)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

; (defun c:DASPdraw-at-section-point (/ *error* *CurDoc* *Old_SysVar*) ;直接在剖面上加点
;   ;--程序初始化--
;     (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
;     (defun *error* (msg)  ;出错函数
;       ;其它需要恢复的内容
;       (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
;       (vla-endundomark *CurDoc*) ;错误时结束编组
;     )

;     (while (eq 8 (logand 8 (getvar 'undoctl))) 
;       (vla-endundomark *CurDoc*)
;     ) ;关闭以前的编组
    
;     (vla-startundomark *CurDoc*) ;记录编组
;       (progn ;记录系统变量
;         (setq *Old_SysVar* '());清空变量,避免出错
;         (setq *Old_SysVar* 
;             (mapcar 
;               '(lambda (modifiedSystemVar / initSystemValue) 
;                 (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
;                     (progn 
;                       (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
;                       (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
;                     )
;                 )
;               )
;               ;此处按格式添加需要改变的系统变量值
;               (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
;             )
;         );记录参数
;       )
;   (draw-at-section-point)
;   ;--程序结尾--
;     (command "redraw");清空一行
;     (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
;     (vla-endundomark *CurDoc*) ;结束编组
;     (princ)
; )


; (defun c:DADPP-draw-at-plan-point (/ *error* *CurDoc* *Old_SysVar*) ;在平面上加点，计算到剖面上的点并加入点列表 
;   ;--程序初始化--
;     (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
;     (defun *error* (msg)  ;出错函数
;       ;其它需要恢复的内容
;       (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
;       (vla-endundomark *CurDoc*) ;错误时结束编组
;     )

;     (while (eq 8 (logand 8 (getvar 'undoctl))) 
;       (vla-endundomark *CurDoc*)
;     ) ;关闭以前的编组
    
;     (vla-startundomark *CurDoc*) ;记录编组
;       (progn ;记录系统变量
;         (setq *Old_SysVar* '());清空变量,避免出错
;         (setq *Old_SysVar* 
;             (mapcar 
;               '(lambda (modifiedSystemVar / initSystemValue) 
;                 (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
;                     (progn 
;                       (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
;                       (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
;                     )
;                 )
;               )
;               ;此处按格式添加需要改变的系统变量值
;               (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
;             )
;         );记录参数
;       )
;   (draw-at-plan-point)
;   ;--程序结尾--
;     (command "redraw");清空一行
;     (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
;     (vla-endundomark *CurDoc*) ;结束编组
;     (princ)
; )

(defun C:FDSA-find-section->plan (/ *error* *CurDoc* *Old_SysVar*) ;通过剖面上的点，获得平面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (find-section->plan)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:FDAS-find-plan->section (/ *error* *CurDoc* *Old_SysVar*) ;通过平面面上的点，获得剖面面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (find-plan->section)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:BakupSectionData (/ *error* *CurDoc* *Old_SysVar*) ;通过平面面上的点，获得剖面面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (bakup-section-data)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:ImportSectionData (/ *error* *CurDoc* *Old_SysVar*) ;通过平面面上的点，获得剖面面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (import-section-data)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

(defun C:RSTRemoveSectionPoint (/ *error* *CurDoc* *Old_SysVar*) ;通过平面面上的点，获得剖面面图对应位置 
  ;--程序初始化--
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)  ;出错函数
      ;其它需要恢复的内容
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)));参数恢复
      (vla-endundomark *CurDoc*) ;错误时结束编组
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    ) ;关闭以前的编组
    
    (vla-startundomark *CurDoc*) ;记录编组
      (progn ;记录系统变量
        (setq *Old_SysVar* '());清空变量,避免出错
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)));获取被修改的系统变量的初始值到{initSystemVar}
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar);修改系统变量值为下面list对应的
                      (list (car modifiedSystemVar) initSystemValue);将被修改的系统变量初始值赋给组表{*Old_SysVar*}
                    )
                )
              )
              ;此处按格式添加需要改变的系统变量值
              (vls-Modify-SystemVariant);TbxlsCore内的函数，获取修改的系统变量列表及值
            )
        );记录参数
      )
  (remove-section-point)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)