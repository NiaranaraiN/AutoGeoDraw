           
	                                 
	                                                                                                                                                                                                                                                                                                                           
	(if (null vlax-dump-object) (vl-load-com))                                     
	(load "TbxlsCore")
	(load "TbxlsIO")
  (load "TbxlsText")
	(load "TbxlsGraphElement")
	
	(setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
	(SETQ *open_path* "W:\\")
  (setq *reset_variants* (list "*set_line_color*"  "*plan_line*"  "*plan_start_point*" "*plan_end_point*" "*plan_line_obj*" "*plan_scale*" "*section_line*" "*section_start_point*" "*section_end_point*" "*section_line_obj*" "*section_scale*" "*section_points_x*" "*structure_points_x" "*strata_count*" "*plan_start_distance*" "*plan_end_distance*" "*section_start_distance*" "*section_end_distance*" "*x_scale*" "*y_scale*" "*section_start_x*" "*set_line_layer*" ))
                                                                                              
                                               
                                      
                           
              
                                              
                                      
                                                     
                                         
        
       
                     
   
      


                                              

(defun C:SOH-section-Order-Help ( / )
  (alert (strcat "命令(SOH)：\n" "初始设置：    InitSectionVariants\n" "重置数据：    ResetAllValue\n" "选剖面线点：AddSectionPoint\n" "选平面线点：AddPlanPoint\n"  "剖面->平面：FDSA-find_section_to_plan \n" "平面->剖面：FDAS-find_plan_to_section \n" "备份数据：    BakupSectionData\n" "恢复数据 ：    ImportSectionData\n" "删除上一点 ：    Remove-last-point\n" "删除点数据 ：    RSTRemoveSectionPoint\n" ))
	
)

                          
(C:SOH-section-Order-Help)

                                               

(defun draw_strata_height_line_now (self.on_ground_point.pt self.strata_height_list.list self.line_obj_list.list / __strata_limits_points.list __draw_line_points.list __curr_point_x.real __height_index.int __strata_height.real __height_list_length.int  __line_maked.ename )                   
  (setq __curr_point_x.real (car self.on_ground_point.pt))
  (setq __strata_height.real (cadr self.on_ground_point.pt))        
  (setq __height_index.int  0 __height_list_length.int (length self.strata_height_list.list))
  (setq *set_line_layer* "Geo_说明_不打印")
  (while (< __height_index.int __height_list_length.int)
		(setq __strata_height.real (- __strata_height.real (* (nth __height_index.int self.strata_height_list.list) *y_scale*)	))
                                                   
		(setq __strata_limits_points.list (append __strata_limits_points.list (list (list __curr_point_x.real __strata_height.real))))
    (if (= __height_index.int 0)
      	(progn
        	(setq __draw_line_points.list (list self.on_ground_point.pt (nth __height_index.int __strata_limits_points.list)))
        	(setq *set_line_color* (1+ __height_index.int))
        	(if (> (nth __height_index.int self.strata_height_list.list) 0)
          	(progn
							(points_draw_polyline __draw_line_points.list)
          		(setq self.line_obj_list.list (append self.line_obj_list.list (list (vlax-ename->vla-object (entlast))) ))
						)
          )
				)
      	(progn
        	(setq __draw_line_points.list (list (nth (1- __height_index.int) __strata_limits_points.list) (nth __height_index.int __strata_limits_points.list)))
        	(setq *set_line_color* (1+ __height_index.int))
        	(if (> (nth __height_index.int self.strata_height_list.list) 0)
						(progn
							(points_draw_polyline __draw_line_points.list)
          		(setq self.line_obj_list.list (append self.line_obj_list.list (list (vlax-ename->vla-object (entlast))) ))
						)
					)
				)
		)
    
		(setq __height_index.int (1+ __height_index.int))
  )
  (vls-obj-list->groupunname self.line_obj_list.list)
)



                                                                                                                                                                 
                          
                                                             
                                                             
                       
                                                                              
                          
                                                          
     
                                  
                                                         
                                                                                                
     
                                                                          
   

                                                                                                                                                                                
                           
                                                          
                                                             
                       
                                                                              
                          
                                                           
     
                                   
                                                         
                                                                                               
     
                                                                              
                                                                                
   


                                                  

                                                          

(defun find_section_to_plan ( / __at_line.ename __curr_point.pt __at_plan_point.pt __get_distance.real __xline_list.list __point_at_line.pt __at_plan_line_distance.real __at_plan_line_point.pt __at_section_line_point.pt __at_section_line_distance.real)                                   
  (setq __point_at_line.pt nil)
  (while (not __point_at_line.pt)             
    (setvar "osmode" 14335)
		(setq __curr_point.pt (getpoint "选择平面或剖面图上的点："))
		(setvar "osmode" 0)
    (setq __at_plan_line_distance.real (vlax-curve-getdistatpoint *plan_line_obj* __curr_point.pt))
    (if __at_plan_line_distance.real
                         
      (progn
        (setq __at_plan_line_point.pt __curr_point.pt)
				(setq __at_section_line_point.pt (plan_point_to_section_point __at_plan_line_point.pt))                
      	(setq __point_at_line.pt T)
      )
                       
      (progn      
        (setq __at_section_line_distance.real (vlax-curve-getdistatpoint *section_line_obj* __curr_point.pt))
                             
        (if __at_section_line_distance.real
					(progn 
						(setq __at_section_line_point.pt __curr_point.pt)
						(setq *section_point_to_start_distance* (- (car __at_section_line_point.pt)  (car *section_start_point*)))
						(setq __at_plan_line_point.pt  (section_distance_to_plan_point *section_point_to_start_distance*))
						(setq __point_at_line.pt T)
					)
				) 
      )
    )
	)
  (progn 
		(setq *set_line_layer* nil *set_line_color* 20)
		(make_xline __at_plan_line_point.pt '(0 1 0))
		(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
		(make_xline __at_plan_line_point.pt '(1 0 0))
		(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
		(make_xline __at_section_line_point.pt '(0 1 0))
		(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
		(vls-obj-list->groupunname __xline_list.list)
	)
     
		                      
		                        
		                          
		                                                      
		                      
		                                                                                                                  
		             
		                                                                             
		                                                                          
		                                                   
		                                           
		                                                                                           
		                                           
		                                                                                           
		                                        
		                                                                                           
		                                                
		                       
		       
		    

			
		   
)

(defun find_plan_to_section ( / __curr_point.pt __x_increment.real __section_point.pt __xline_listz.list __point_at_line.pt __xline_list.list)                                       
  (setq __point_at_line.pt nil)
  (while (not __point_at_line.pt)
		(setvar "osmode" 14335)
		(setq __curr_point.pt (getpoint "选择平面图上的点："))
		(setvar "osmode" 0)
		(if (setq __point_at_line.pt (vlax-curve-getdistatpoint *plan_line_obj* __curr_point.pt))
			(progn
     		(setq __section_point.pt (plan_point_to_section_point __curr_point.pt))
				(setq *set_line_layer* nil *set_line_color* 20)
				(make_xline __section_point.pt '(0 1 0))
				(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
				(make_xline __curr_point.pt '(0 1 0))
				(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
				(make_xline __curr_point.pt '(1 0 0))
				(setq __xline_list.list (append __xline_list.list (list (vlax-ename->vla-object (entlast)))))
				(vls-obj-list->groupunname __xline_list.list)
				(setq __point_at_line.pt T)
			)
		)
	)
)

                                                        

                                                        

(defun bakup_variants_to_file_line (self.variant_str.str self.bakup_file.str / __list_str.list __list_item.list)
	(cond 
		( (equal (type (eval (read self.variant_str.str))) 'INT) (write-line (strcat "INT" ":"  self.variant_str.str ":" (itoa (eval (read self.variant_str.str) ))) *bakup_file*) )
		( (equal (type (eval (read self.variant_str.str))) 'REAL) (write-line (strcat "REAL" ":"  self.variant_str.str ":" (rtos (eval (read self.variant_str.str))  2 16) ) *bakup_file*) )
		( (equal (type (eval (read self.variant_str.str))) 'STR) (write-line (strcat "STR" ":"  self.variant_str.str ":" (eval (read self.variant_str.str ))) *bakup_file*) )
		( (equal (type (eval (read self.variant_str.str))) 'ENAME) (write-line (strcat "ENAME" ":"  self.variant_str.str ":" (cdr (assoc 5  (entget (eval (read self.variant_str.str )))))) *bakup_file*) )
		( (equal (type (eval (read self.variant_str.str))) 'VLA-OBJECT)
			(progn 
     		(write-line (strcat "VLA-OBJECT" ":"  self.variant_str.str ":" (vla-get-handle (eval (read self.variant_str.str))) ) *bakup_file*)
      )
		)
		( (equal (type (eval (read self.variant_str.str))) 'LIST)
			(progn 
				(foreach __list_item.list (eval (read self.variant_str.str))
					(if (not __list_str.list)
							(setq __list_str.list  (rtos __list_item.list 2 16))
							(setq __list_str.list (strcat __list_str.list "," (rtos __list_item.list 2 16)))
					))
    		(write-line (strcat "LIST" ":"  self.variant_str.str ":" __list_str.list) *bakup_file*)
      )
		)
  )
)

(defun import_variants_from_file_line (self.raw_data.str / __get_variants.list __var_type.int __var_name.str __var_data.any __recovery_list.list __get_list_str.list __list_item.list)
	(setq __get_variants.list (vls-string->list self.raw_data.str ":"))
	(setq __var_type.int (car __get_variants.list) __var_name.str (cadr __get_variants.list) __var_data.any (caddr __get_variants.list))
	(cond 
		((= __var_type.int "INT") (set (read __var_name.str) (atoi __var_data.any)) )
		((= __var_type.int "REAL") (set (read __var_name.str) (atof __var_data.any) ))
		((= __var_type.int "STR") (set (read __var_name.str) __var_data.any))
		((= __var_type.int "ENAME") (set (read __var_name.str) (handent __var_data.any)) )
		((= __var_type.int "VLA-OBJECT") (set (read __var_name.str) (vlax-ename->vla-object (handent __var_data.any))) )
		((= __var_type.int "LIST")
     (progn 
      (setq __get_list_str.list (vls-string->list __var_data.any ","))
      (foreach __list_item.list __get_list_str.list
				(setq __recovery_list.list  (append __recovery_list.list (list (atof __list_item.list))))
      )
     	(set (read __var_name.str) __recovery_list.list)
     ))
	)
)


(defun import_section_data ( / __split_path.str __path_index.int __recovery_file.file __data_index.int __temp_file.file __temp_data.any __temp_point_str.pt  __bak_path.str __only_path.str __line_data.ename __temp_point.pt)          
  (reset_all_value)
  (alert "【导入恢复】数据")
	(setq __bak_path.str (getfiled "导入剖面数据" *open_path* "vlsdata" 16))
  (setq __split_path.str (vls-string->list __bak_path.str "\\"))
  (setq __only_path.str nil __path_index.int 0)
  (while (< __path_index.int (1- (length __split_path.str)))
    (if (not __only_path.str)
			(setq __only_path.str (nth __path_index.int __split_path.str) )
    	(SETQ __only_path.str (strcat __only_path.str "\\" (nth __path_index.int __split_path.str)))
		)
    (setq __path_index.int (1+	__path_index.int))
  ) 
  (setq __recovery_file.file (open __bak_path.str "r"))
  (while  (setq __line_data.ename (read-line __recovery_file.file))
					(import_variants_from_file_line __line_data.ename)
	) 
  (setq __data_index.int 0 )
  (while (< __data_index.int *strata_count*)
  	(setq __temp_file.file (open (strcat __only_path.str "\\strata_points_" (itoa __data_index.int) ".vlsdata")  "r"))
    (while (setq __temp_data.any (read-line __temp_file.file))
    	(setq __temp_point_str.pt (vls-string->list __temp_data.any ","))
    	(setq __temp_point.pt (list (atof (car __temp_point_str.pt)) (atof (cadr __temp_point_str.pt)) (atof (caddr __temp_point_str.pt)) ))
      (set (read (strcat "*strata_points_" (itoa __data_index.int) "*")) (append (eval (read (strcat "*strata_points_" (itoa __data_index.int) "*"))) (list __temp_point.pt)))
  	)
    (close __temp_file.file)
    (setq __data_index.int (1+ __data_index.int))
	)  
	(setq __temp_file.file (open (strcat __only_path.str "\\ground_points" ".vlsdata")  "r"))
	(while (setq __temp_data.any (read-line __temp_file.file))
		(setq __temp_point_str.pt (vls-string->list __temp_data.any ","))
		(setq __temp_point.pt (list (atof (car __temp_point_str.pt)) (atof (cadr __temp_point_str.pt)) (atof (caddr __temp_point_str.pt)) ))
		(setq *ground_points*  (append *ground_points* (list __temp_point.pt)))
	)
  (close __temp_file.file)
  (setq *open_path* __bak_path.str)
  (alert "恢复成功")
)

(defun bakup_section_data ( / __split_path.str __path_index.int __bakup_var.any __bakup_file.file __data_index.int __temp_file.file __point_data.pt __bak_path.str __only_path.str __point_data.pt)                                                              
	(alert "【备份】数据")
	(setq __bak_path.str (getfiled "备份剖面数据" *open_path* "vlsdata" 33))
  (setq __split_path.str (vls-string->list __bak_path.str "\\"))
  (setq __only_path.str nil __path_index.int 0)
  (while (< __path_index.int (1- (length __split_path.str)))
    (if (not __only_path.str)
			(setq __only_path.str (nth __path_index.int __split_path.str) )
    	(SETQ __only_path.str (strcat __only_path.str "\\" (nth __path_index.int __split_path.str)))
		)
    (setq __path_index.int (1+	__path_index.int))
  )
  
  (setq __bakup_file.file (open __bak_path.str "w"))
  (foreach __bakup_var.any *reset_variants*
    (bakup_variants_to_file_line __bakup_var.any __bakup_file.file)
	)
  (close __bakup_file.file)
  
  
  (setq __data_index.int 0 )
  (while (< __data_index.int *strata_count*)
  	(setq __temp_file.file (open (strcat __only_path.str "\\strata_points_" (itoa __data_index.int) ".vlsdata")  "w"))
    (foreach __point_data.pt (eval (read (strcat "*strata_points_" (itoa __data_index.int) "*")))
			(write-line (strcat (rtos (car __point_data.pt) 2 16) "," (rtos (cadr __point_data.pt) 2 16) "," (rtos (caddr __point_data.pt) 2 16)) __temp_file.file)
    )
    (close __temp_file.file)
		(setq __data_index.int  (1+ __data_index.int))
	)
  
  
  (setq __temp_file.file (open (strcat __only_path.str "\\ground_points" ".vlsdata")  "w"))
    (foreach __point_data.pt *ground_points*
			(write-line (strcat (rtos (car __point_data.pt) 2 16) "," (rtos (cadr __point_data.pt) 2 16) "," (rtos (caddr __point_data.pt) 2 16)) __temp_file.file)
    )
    (close __temp_file.file)
  (setq *open_path* __bak_path.str)
  (alert "备份完成")
)

                                                      




(defun vls-obj-list->groupunname (self.obj_list.list / __curr_group_dict.any __new_group.any )                                                    
	(vla-AppendItems
		(vla-add (vla-get-Groups (vla-get-activedocument (vlax-get-acad-object))) "*")
		(vlax-safearray-fill (vlax-make-safearray vlax-vbObject (cons 0 (1- (length self.obj_list.list)))) self.obj_list.list )
	)
)

(defun vls-obj-list->named-group (self.obj_list.list self.group_name.str / __curr_group_dict.any __new_group.any )                                                    
	(vla-AppendItems
		(vla-add (vla-get-Groups (vla-get-activedocument (vlax-get-acad-object))) self.group_name.str)
		(vlax-safearray-fill (vlax-make-safearray vlax-vbObject (cons 0 (1- (length self.obj_list.list)))) self.obj_list.list )
	)
)

(defun vls-string->list (self.recv_string.str self.split_char.any / __pos.any)                  
	(if (setq __pos.any (VL-STRING-SEARCH self.split_char.any self.recv_string.str))
		(PROGN (CONS (SUBSTR self.recv_string.str 1 __pos.any) (vls-string->list (substr self.recv_string.str (+ __pos.any 1 (STRLEN self.split_char.any))) self.split_char.any)))
		(PROGN (LIST self.recv_string.str))
	)	
)

(defun line_intersectionwith (self.line_obj1.obj self.line_obj2.obj / __result.any __inter_result.any)                                                                                                                                                         
  (setq __inter_result.any (vlax-variant-value (vlax-invoke-method self.line_obj1.obj 'IntersectWith self.line_obj2.obj 0)))
  (setq __result.any (vlax-safearray->list __inter_result.any))
  __result.any
)

(defun reset_all_value ( / __one_var.any __list_index.int __list_length.int __reset_points_list.list __one_var.any)

  (setq __reset_points_list.list (LIST "*ground_points*" ))
	(setq __list_index.int 0)
	(while (< __list_index.int *strata_count*)           
		(set (read (strcat "*strata_points_" (itoa __list_index.int) "*")) nil)
		(set (read (strcat "*strata_points_increment_" (itoa __list_index.int) "*")) nil)
		(setq __list_index.int (1+ __list_index.int))
	)
  (setq *strata_lines_handle* nil)
  (foreach __one_var.any  *reset_variants*
		(set (read __one_var.any) nil)
  )
  (foreach __one_var.any  __reset_points_list.list
		(set (read __one_var.any) nil)
  )
  (alert "初始化成功")
                          
)



(defun section_distance_to_plan_point (self.section_point_x_distance.real / __result.any __plan_distance.real )                                                
  (setq __plan_distance.real  (abs (- *plan_start_distance* (/ self.section_point_x_distance.real *x_scale*) )))
  (setq __result.any (vlax-curve-getPointAtDist *plan_line_obj* __plan_distance.real))
  __result.any
)

(defun plan_distance_to_section_distance (self.point.pt / __result.any)                                                                 
  (setq __result.any (abs (- *plan_start_distance* (vlax-curve-getdistatpoint *plan_line_obj* self.point.pt) )))
  (setq __result.any  (* __result.any *x_scale*))
  __result.any
)



(defun plan_point_to_section_point (self.plan_point.pt / __section_distance.real __ground_x.real __xline_for_inter.ename __xline_for_inter_obj.obj __inter_point.pt)                               
  (setq __section_distance.real (plan_distance_to_section_distance self.plan_point.pt))                           
  (setq __ground_x.real (+ *section_start_x* __section_distance.real))
  (make_xline (list __ground_x.real (cadr *section_start_point*) 0) '(0 1 0))
  (setq __xline_for_inter.ename  (entlast))
  (setq __xline_for_inter_obj.obj (vlax-ename->vla-object __xline_for_inter.ename))
  (setq __inter_point.pt (line_intersectionwith *section_line_obj* __xline_for_inter_obj.obj))
  (entdel __xline_for_inter.ename)
  __inter_point.pt
	
)

(defun get_remove_element_index (self.recv_list.list self.remove_element.ename / __remove_index.int __index_list.int __find_index.int __result.any __one_element.ename)                                                                                    
  (setq __remove_index.int 0 __index_list.int nil __find_index.int nil __result.any nil)
  (while (and (not __find_index.int) (< __remove_index.int (length self.recv_list.list)))
		(setq __one_element.ename (nth __remove_index.int self.recv_list.list))
    (if (equal __one_element.ename self.remove_element.ename)
			(progn 
				(setq __result.any __remove_index.int)
				(setq __find_index.int T)
			)
			(setq __remove_index.int (1+ __remove_index.int))
		)
  )
  __result.any
)


(defun c:remove-last-point ( / __strata_index.int __ss_index.int  __ename.str)
	(setq *section_points_x* (cdr *section_points_x*))
	(setq *ground_points* (cdr *ground_points*))
  (setq __strata_index.int  0)
  (while (< __strata_index.int *strata_count*)
    (set (read (strcat "*strata_points_" (itoa __strata_index.int) "*"))
     		 (cdr (eval (read (strcat "*strata_points_" (itoa __strata_index.int) "*")))))
  	(setq __strata_index.int (1+ __strata_index.int)) 
  )
                   
  (if (>= (length *section_points_x*) 2)
    (progn 
			(if (> (length *strata_lines_handle*) 0)
				(progn
				(setq __ss_index.int  0)
					(while (< __ss_index.int (length *strata_lines_handle*))
						(setq __ename.str (handent (nth __ss_index.int *strata_lines_handle*)) )
						(entdel __ename.str)
						(setq __ss_index.int (1+ __ss_index.int))
					)
				)
			)
			(draw_all_section_line)
    )
	)
)

(defun remove_section_point ( / __point_at_line.pt __curr_point.pt __at_plan_line_distance.real  __at_plan_line_point.pt __at_section_line_point.pt __at_section_line_distance.real __section_point_to_start_distance.real __ss_index.int __ename.str  __remove_index.int __strata_index.int __remove_point.pt)
	(setq __point_at_line.pt nil)
	(while (not __point_at_line.pt)                                                    
		(setvar "osmode" 14335)
		(setq __curr_point.pt (getpoint "选择要删除的点："))
		(setvar "osmode" 0)
		(setq __at_plan_line_distance.real (vlax-curve-getdistatpoint *plan_line_obj* __curr_point.pt))
		(if __at_plan_line_distance.real
			                   
			(progn
				(setq __at_plan_line_point.pt __curr_point.pt)
				(setq __at_section_line_point.pt (plan_point_to_section_point __at_plan_line_point.pt))                
				(setq __point_at_line.pt T)
			)
			                 
			(progn      
				(setq __at_section_line_distance.real (vlax-curve-getdistatpoint *section_line_obj* __curr_point.pt))
				                     
				(if __at_section_line_distance.real
					(progn 
						(setq __at_section_line_point.pt __curr_point.pt)
						(setq __point_at_line.pt T)
					)
				) 
			)
		)
	)
  (setq __remove_index.int (get_remove_element_index *ground_points* __at_section_line_point.pt))
  (if (not __remove_index.int)
		(alert "初始未找到，删除完成！")
		(progn
			(setq __remove_point.pt (nth __remove_index.int *ground_points*))
			(setq *ground_points* (vl-remove __remove_point.pt *ground_points*))
			(setq *section_points_x* (vl-remove (car __remove_point.pt) *section_points_x*))
			(setq __strata_index.int  0)
			(while (< __strata_index.int *strata_count*)
				(setq __remove_point.pt (nth __remove_index.int (eval (read (strcat "*strata_points_" (itoa __strata_index.int) "*")))) )
				(set
          (read (strcat "*strata_points_" (itoa __strata_index.int) "*"))
					(vl-remove __remove_point.pt (eval (read (strcat "*strata_points_" (itoa __strata_index.int) "*"))))
        )
				(setq __strata_index.int (1+ __strata_index.int))
			)
    	(alert "已找到，删除完成！")
    )
	)
	                 
  (if (>= (length *section_points_x*) 2)
    (progn 
			(if (> (length *strata_lines_handle*) 0)
				(progn
				(setq __ss_index.int  0)
					(while (< __ss_index.int (length *strata_lines_handle*))
						(setq __ename.str (handent (nth __ss_index.int *strata_lines_handle*)) )
						(entdel __ename.str)
						(setq __ss_index.int (1+ __ss_index.int))
					)
				)
			)
			(draw_all_section_line)
    )
	)
  
  
  
)

(defun append_in_section_data (self.on_ground_point.pt self.strata_height_list.list / __curr_point_x.real __height_index.int __strata_height.real __height_list_length.int )                   
  (setq *ground_points* (append (list self.on_ground_point.pt) *ground_points*))
  (setq __curr_point_x.real (car self.on_ground_point.pt))
	(setq *section_points_x* (append (list __curr_point_x.real) *section_points_x* ))
  (setq __strata_height.real (cadr self.on_ground_point.pt))        
  (setq __height_index.int  0 __height_list_length.int (length self.strata_height_list.list))
  (while (< __height_index.int __height_list_length.int)
                           
		(setq __strata_height.real (- __strata_height.real (* (nth __height_index.int self.strata_height_list.list) *y_scale*)	))
                                                   
    (set (read (strcat "*strata_points_" (itoa __height_index.int) "*")) (append (list (list __curr_point_x.real __strata_height.real 0.0)) (eval (read (strcat "*strata_points_" (itoa __height_index.int) "*")))))
		(setq __height_index.int (1+ __height_index.int))
  )
)


(defun add_section_point ( / __point_at_line.pt __strata_height_list.list  __curr_point.pt __strata_height_all.real __strata_height_str.real __correct_imput.any __correct_input.any __strata_height_list_str.list __section_point_to_start_distance.real __at_plan_line_distance.real __at_plan_line_point.pt __at_section_line_point.pt __at_section_line_distance.real __plan_point_maked_obj.obj __group_objects.obj __ss_index.int __ename.str  )                 
  (setq __point_at_line.pt nil)
  (while (not __point_at_line.pt)             
    (setvar "osmode" 14335)
		(setq __curr_point.pt (getpoint "选择平面或剖面图上的点："))
		(setvar "osmode" 0)
    (setq __at_plan_line_distance.real (vlax-curve-getdistatpoint *plan_line_obj* __curr_point.pt))
    (if __at_plan_line_distance.real
                         
      (progn
        (setq __at_plan_line_point.pt __curr_point.pt)
				(setq __at_section_line_point.pt (plan_point_to_section_point __at_plan_line_point.pt))                
      	(setq __point_at_line.pt T)
      )
                       
      (progn      
        (setq __at_section_line_distance.real (vlax-curve-getdistatpoint *section_line_obj* __curr_point.pt))
                             
        (if __at_section_line_distance.real
					(progn 
						(setq __at_section_line_point.pt __curr_point.pt)
						(setq __section_point_to_start_distance.real (- (car __at_section_line_point.pt)  (car *section_start_point*)))
						(setq __at_plan_line_point.pt  (section_distance_to_plan_point __section_point_to_start_distance.real))
						(setq __point_at_line.pt T)
					)
				) 
      )
    )
	)
  (setq __correct_input.any  nil)
  (while (not __correct_input.any) 
  	(setq __strata_height_all.real (getstring (strcat "输入" (itoa *strata_count*) "层岩土层厚度：")))
  	(setq __strata_height_list_str.list (vls-string->list __strata_height_all.real ","))
    (setq __correct_input.any (equal (length __strata_height_list_str.list) *strata_count*))
	)
	(setq __strata_height_list.list nil)
  (foreach __strata_height_str.real __strata_height_list_str.list
		(setq __strata_height_list.list (append  __strata_height_list.list (list (atof __strata_height_str.real))))
  )
  (append_in_section_data __at_section_line_point.pt __strata_height_list.list)
  (entmake (list
            	(cons 0 "POINT")
            	(cons 10 __at_plan_line_point.pt)
							'(8 . "Temp")
							'(62 . 20)
           )	
  )
  (setq __plan_point_maked_obj.obj (vlax-ename->vla-object (entlast)))
  (setq __group_objects.obj (append __group_objects.obj (list __plan_point_maked_obj.obj)))
  (draw_strata_height_line_now __at_section_line_point.pt __strata_height_list.list __group_objects.obj)

  
                 
  (if (>= (length *section_points_x*) 2)
    (progn 
			(if (> (length *strata_lines_handle*) 0)
				(progn
				(setq __ss_index.int  0)
					(while (< __ss_index.int (length *strata_lines_handle*))
						(setq __ename.str (handent (nth __ss_index.int *strata_lines_handle*)) )
						(entdel __ename.str)
						(setq __ss_index.int (1+ __ss_index.int))
					)
				)
			)
			(draw_all_section_line)
    )
	)
  (alert "添加成功")
)

(defun add_plan_point ( /)                                           
  (add_section_point)
                           
                              
                                
	                                                      
	                                                                                 
     
                       
                               
                                 
	                                                                                               
	                                                                             
                                                                                    
	   
                                   
                                                         
	                                                                                              
     
                                                                       
                                                                     
                                                                          
                      
)

(defun add_structure_point ( / )         
  (setvar "osmode" 14335)
	(setq *structure_point* (getpoint "选择地质特征点："))
  (setq *structure_name* (getstring "输入地质点描述"))
                                                                                        
  (setq *structure_distance* (plan_distance_to_section_distance *structure_point*))
  (setq *structure_name_list*     (append (list *structure_name*) *structure_name_list* ))
  (setq *structure_distance_list* (append (list *structure_distance*) *structure_distance_list* ))
  (setvar "osmode 0")
)

(defun select_section_lines ( / __is_success.any )           
                           
  (setq *strata_count* nil)
  (while (not *strata_count*) 
  	(setq *strata_count* (getint "输入岩土层层数："))
	)
  (setq __is_success.any  0 *plan_line* nil)
  (while (= __is_success.any 0)
    (setvar "osmode" 1573)
		(setq *plan_line* (car (entsel "选择平面图上的剖面线：")))
    (if (or (= (cdr (assoc 0 (entget *plan_line*))) "LWPOLYLINE") (= (cdr (assoc 0 (entget *plan_line*))) "POLYLINE") (= (cdr (assoc 0 (entget *plan_line*))) "LINE") )
			(progn
				(setq *plan_line_obj* (vlax-ename->vla-object *plan_line*))           
				(setq __is_success.any 1)
      ))
    (setvar "osmode" 0)
	)
  (setq __is_success.any 0)
	(while (= __is_success.any 0)
		(setvar "osmode" 1573)
		(setq *plan_start_point* (getpoint "选择起点坐标："))
		(setq *plan_end_point* (getpoint "选择终点坐标："))
		(setq *plan_start_distance* (vlax-curve-getdistatpoint *plan_line_obj* *plan_start_point*) )                                 
		(setq *plan_end_distance* (vlax-curve-getdistatpoint *plan_line_obj* *plan_end_point*) )                                 
		(if (not (or (not *plan_start_distance*) (not *plan_end_distance*)))
			(progn
				(setq *plan_scale* 1000.0)
				(setq *plan_scale* (getreal "平面图比例1：<1000>"))
				(if (not *plan_scale*) (setq *plan_scale* 1000.0))
				(setq __is_success.any 1)
			)
		)
   	(setvar "osmode" 0)
   ) 
  (setq __is_success.any  0 *section_line* nil)
  (while (= __is_success.any 0)
		(setvar "osmode" 14335)
		(setq *section_line* (car (entsel "选择剖面图上的剖面线：")))
    (if (or (= (cdr (assoc 0 (entget *section_line*))) "LWPOLYLINE") (= (cdr (assoc 0 (entget *section_line*))) "POLYLINE") (= (cdr (assoc 0 (entget *section_line*))) "LINE"))
				(setq __is_success.any 1)
    )
		(setvar "osmode" 0)
	)
	(setq *section_line_obj* (vlax-ename->vla-object *section_line*))           
  (setq __is_success.any  0 )
	(while (= __is_success.any 0)
		(setvar "osmode" 14335)
		(setq *section_start_point* (getpoint "选择起点坐标："))
		(setq *section_end_point* (getpoint "选择终点坐标："))
		(setq *section_start_distance* (vlax-curve-getdistatpoint *section_line_obj* *section_start_point*) )                                 
		(setq *section_end_distance* (vlax-curve-getdistatpoint *section_line_obj* *section_end_point*) )                                 
		(if (not (or (not *section_start_distance*) (not *section_end_distance*)))
      (progn
        (setq *section_scale* (getreal "剖面图比例1：<1000>"))
        (if (not *section_scale*) (setq *section_scale* 1000.0))
				(setq __is_success.any 1)

			)
    )
		(setvar "osmode" 0)
	)
  (setq *x_scale* (/ *plan_scale* *section_scale*))               
  (setq *y_scale* (/ 1000.0 *section_scale*))
  (setq *section_start_x* (car *section_start_point*))         

)


(defun strata_points_interpolation ( / __list_index.int __delta_point.pt __next_point.pt)                             
	(defun insert_increment_points ( self.current_point.pt self.next_point.pt self.ground_start_point.pt self.selt_ground_end_point.pt / __point_index.int __interpolation_points.list  __x_increment.real __y_increment.real __delta_x.real __delta_y.real __count_delta.int __start_xline.ename __start_xline_obj.obj __end_xline.ename __end_xline_obj.obj __start_difference_y.real __end_difference_y.real __difference_y_increment.real __delta_point_x.real __delta_xline_point.pt __inter_xline.ename __inter_xline_obj.obj __inter_groud_point.pt )                                                                                   
    (setq  __start_difference_y.real (- (cadr self.ground_start_point.pt) (cadr self.current_point.pt)))                                     
    (setq __end_difference_y.real (- (cadr self.selt_ground_end_point.pt) (cadr self.next_point.pt)))
    (setq __difference_y_increment.real (- __end_difference_y.real __start_difference_y.real) )
    (setq __x_increment.real (- (car self.next_point.pt) (car self.current_point.pt)) __y_increment.real (- __end_difference_y.real __start_difference_y.real))              
		(cond
			((>= __x_increment.real 2000) (setq __delta_x.real 50.0))
			((>= __x_increment.real 1000) (setq __delta_x.real 30.0))
			((>= __x_increment.real 500 ) (setq __delta_x.real 20.0))
			((>= __x_increment.real 100 ) (setq __delta_x.real 10.0))
			((>= __x_increment.real 10  ) (setq __delta_x.real 2.0 ))
			((>= __x_increment.real 5   ) (setq __delta_x.real 0.5 ))
			((> __x_increment.real 0    ) (setq __delta_x.real 0.25))
    )
    (setq __delta_y.real (/ __y_increment.real (/ __x_increment.real __delta_x.real)) __count_delta.int 0 )
    (while (< (+ (car self.current_point.pt) (* __delta_x.real  __count_delta.int)) (car self.next_point.pt) )
      (setq __delta_point_x.real (+ (car self.current_point.pt) (* __delta_x.real  __count_delta.int)))
      (setq __delta_xline_point.pt (list __delta_point_x.real  (cadr self.current_point.pt) 0))
      (make_xline __delta_xline_point.pt '(0 1 0))
      (setq __inter_xline.ename (entlast))
      (setq __inter_xline_obj.obj (vlax-ename->vla-object __inter_xline.ename))
      (setq __inter_groud_point.pt (line_intersectionwith *section_line_obj* __inter_xline_obj.obj))
      (setq __interpolation_points.list	
				(append __interpolation_points.list 
              	(list 
                  (list 
                    (car __inter_groud_point.pt)  
                    (- (- (cadr __inter_groud_point.pt) __start_difference_y.real ) (* __delta_y.real __count_delta.int))
										0.0
                	))  
			))
      (setq __count_delta.int (1+ __count_delta.int))
      (entdel __inter_xline.ename)
    )
                            
                          
    __interpolation_points.list
  )  
  (defun get_points_interpolation_list ( self.points.list / __sorted_index.int __current_point_index.int __next_point_index.int __current_point.pt  __result.any __next_ground_point.pt __current_ground_point.pt)                                           
		(setq __sorted_index.int 0)                 
		(while (< __sorted_index.int (length *list_sorted_index*))
			(setq __current_point_index.int (nth __sorted_index.int *list_sorted_index*))                           
			(if (>= (1+ __sorted_index.int) (length *list_sorted_index*))
				(setq __result.any (append __result.any (list (nth __current_point_index.int self.points.list))))
				(progn
					(setq __next_point_index.int (nth (1+ __sorted_index.int) *list_sorted_index*))                 
					(setq __current_point.pt (nth __current_point_index.int self.points.list))
					(setq __next_point.pt (nth __next_point_index.int self.points.list))
					(setq __current_ground_point.pt (nth __current_point_index.int *ground_points*))
					(setq __next_ground_point.pt (nth __next_point_index.int *ground_points*))
					(setq __result.any (append __result.any (insert_increment_points __current_point.pt __next_point.pt __current_ground_point.pt __next_ground_point.pt)))
        )
			)
		(setq __sorted_index.int (1+ __sorted_index.int))
		)
    __result.any
	)
  
  (setq __list_index.int 0)
	(while (< __list_index.int *strata_count*)           
		(set (read (strcat "*strata_points_increment_" (itoa __list_index.int) "*")) (get_points_interpolation_list (eval (read (strcat "*strata_points_" (itoa __list_index.int) "*")))))
		(setq __list_index.int (1+ __list_index.int))
	)
	
)

(defun points_draw_polyline (self.strata_points.list / __strata_polyline.str __cons_point.pt __one_point.pt __sorted_index.int __strata_point.pt __cons_points.list __result.any ) 
  (setq __strata_polyline.str
		(list
			'(0 . "LWPOLYLINE")                                  
			'(100 . "AcDbEntity")
			'(100 . "AcDbPolyline")
			(cons 90 (length self.strata_points.list))             
			'(70 . 0)                                      
		)
	)
  (if (not *set_line_color*)
    (setq *set_line_color* 7)
		(setq __strata_polyline.str (append  __strata_polyline.str (list (cons 62 *set_line_color*))))
	)
  (if (not *set_line_layer*)
		(setq *set_line_layer* "Temp")
		(setq __strata_polyline.str (append  __strata_polyline.str (list (cons 8 *set_line_layer*))))
	)
  (setq __cons_points.list (mapcar '(lambda (self.one_point.pt) (cons 10 self.one_point.pt)) self.strata_points.list))
  (foreach __strata_point.pt __cons_points.list
    (setq __strata_polyline.str (append __strata_polyline.str (list __strata_point.pt)))
	)
  (setq __result.any (entmake __strata_polyline.str))
  __result.any
)

(defun make_xline (self.xline_center_point.pt self.verctor_point.pt / __maked_xline.ename __result.any)                                                                                 
	(setq __maked_xline.ename
		(list
			'(0 . "XLINE")            
			'(100 . "AcDbEntity")
			'(100 . "AcDbXline")
			(cons 10 self.xline_center_point.pt)   
			(cons 11 self.verctor_point.pt)
		)
	)
    (if (not *set_line_color*)
    (setq *set_line_color* 7)
		(setq __maked_xline.ename (append  __maked_xline.ename (list (cons 62 *set_line_color*))))
	)
  (if (not *set_line_layer*)
		(setq *set_line_layer* "Temp")
		(setq __maked_xline.ename (append  __maked_xline.ename (list (cons 8 *set_line_layer*))))
	)
	(setq __result.any (entmake __maked_xline.ename))
  __result.any
)

                                                    

(defun c:test ( / __list_index.int __data_index.int __bak_path.str __only_path.str __line_data.ename __temp_file.file __temp_data.any __temp_point_str.pt __temp_point.pt __point_data.pt __split_path.str __path_index.int __import_file.file __get_variants.list __var_type.int __var_name.str __var_data.any)
	(setq __bak_path.str (getfiled "备份剖面数据" "W:\\" "vlsdata" 16))
  (setq __split_path.str (vls-string->list __bak_path.str "\\"))
  (setq __only_path.str nil __path_index.int 0)
  (while (< __path_index.int (1- (length __split_path.str)))
    (if (not __only_path.str)
			(setq __only_path.str (nth __path_index.int __split_path.str) )
    	(SETQ __only_path.str (strcat __only_path.str "\\" (nth __path_index.int __split_path.str)))
		)
    (setq __path_index.int (1+	__path_index.int))
  )
  (princ __only_path.str)
  (setq *bakup_file* (open __bak_path.str "w"))
  (write-line "STR:test_var_str:string" *bakup_file*)
  (write-line "REAL:test_var_real:3.14159" *bakup_file*)
  (write-line "INT:test_var_int:3" *bakup_file*)
  (progn
                                                                                                                                                    
  	(write-line "LIST:test_var_list:((1 2) (3 4))" *bakup_file*)
	)
  (write-line "ENAME:test_var_ename:2CA2" *bakup_file*)
  (CLOSE *bakup_file*)
  
  (setq __data_index.int 0 )
  (while (< __data_index.int *strata_count*)
  	(setq __temp_file.file (open (strcat __only_path.str "\\strata_points_" (itoa __data_index.int) ".vlsdata")  "w"))
    (foreach __point_data.pt (eval (read (strcat "*strata_points_" (itoa __data_index.int) "*")))
			(write-line (strcat (rtos (car __point_data.pt) 2 16) "," (rtos (cadr __point_data.pt) 2 16) "," (rtos (caddr __point_data.pt) 2 16)) __temp_file.file)
    )
    (close __temp_file.file)
		(setq __data_index.int  (1+ __data_index.int))
	)
  
                          
  (setq __import_file.file (open __bak_path.str "r"))
  (while (setq __line_data.ename (read-line __import_file.file))
    (setq __get_variants.list (vls-string->list __line_data.ename ":"))
    (setq __var_type.int (car __get_variants.list) __var_name.str (cadr __get_variants.list) __var_data.any (caddr __get_variants.list))
		(cond 
			((= __var_type.int "STR") (set (read __var_name.str) __var_data.any))
			((= __var_type.int "REAL") (set (read __var_name.str) (atof __var_data.any) ))
			((= __var_type.int "INT") (set (read __var_name.str) (atoi __var_data.any)))
			((= __var_type.int "LIST") (set (read __var_name.str) (read (strcat (eval __var_data.any)))))
			((= __var_type.int "ENAME") (set (read __var_name.str) (handent __var_data.any)))
    )
  )
  (close __import_file.file)
  
  (setq __data_index.int 0 )
  (while (< __data_index.int *strata_count*)
  	(setq __temp_file.file (open (strcat __only_path.str "\\strata_points_" (itoa __data_index.int) ".vlsdata")  "r"))
    (while (setq __temp_data.any (read-line __temp_file.file))
    	(setq __temp_point_str.pt (vls-string->list __temp_data.any ","))
    	(setq __temp_point.pt (list (atof (car __temp_point_str.pt)) (atof (cadr __temp_point_str.pt)) (atof (caddr __temp_point_str.pt)) ))
      (set (read (strcat "import_strata_points_" (itoa __data_index.int))) (append (eval (read (strcat "import_strata_points_" (itoa __data_index.int)))) (list __temp_point.pt)))
  	)
    (close __temp_file.file)
    (setq __data_index.int (1+ __data_index.int))
    )
    
)


(defun draw_all_section_line ( / __list_index.int  __last_line_obj.obj __strata_lines_ohj.str)
  (if (< (length *section_points_x*) 2)
		(progn (alert "剖面点过少，至少需要两点！") (exit) )
  )
  (setq __list_index.int 0)
  (setq *list_sorted_index* (vl-sort-i *section_points_x* <))
	(while (< __list_index.int *strata_count*)
    (set (read (strcat "*strata_points_increment_" (itoa __list_index.int) "*")) nil)
  	(setq __list_index.int (1+ __list_index.int))
	)
  (strata_points_interpolation)
  (setq __list_index.int 0 *set_line_layer* "Geo_综合" *set_line_color* 7 *strata_lines_handle* '())

  (while (< __list_index.int *strata_count*)
  	(points_draw_polyline (eval (read (strcat "*strata_points_increment_" (itoa __list_index.int) "*"))))
		(setq  *strata_lines_handle* (append *strata_lines_handle* (list (cdr (assoc 5 (entget (entlast)))))))
  	(setq __list_index.int (1+ __list_index.int))
	)
)
                               



(defun C:DrawAllSectionLine (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                         
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (draw_all_section_line)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)


         
(defun C:AddPlanPoint (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (add_plan_point)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:AddSectionPoint (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (add_section_point)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:InitSectionVariants (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (select_section_lines)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:AddStructurePoint (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (add_structure_point)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:ResetAllValue (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (reset_all_value)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

                                                                                        
                   
                                                                     
                                     
                           
                                                                                                           
                                                  
       

                                                 
                                  
                       
    
                                            
                            
                                                   
                             
                      
                                                               
                                                                                                                           
                             
                                                                                                     
                                                                                                                     
                       
                   
                 
                                                   
                                                                                        
               
                    
         
                           
                 
                                 
                                                                             
                                          
             
   


                                                                                                                  
                   
                                                                     
                                     
                           
                                                                                                           
                                                  
       

                                                 
                                  
                       
    
                                            
                            
                                                   
                             
                      
                                                               
                                                                                                                           
                             
                                                                                                     
                                                                                                                     
                       
                   
                 
                                                   
                                                                                        
               
                    
         
                        
                 
                                 
                                                                             
                                          
             
   

(defun C:FDSA-find-section->plan (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                     
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (find_section_to_plan)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:FDAS-find-plan->section (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                         
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (find_plan_to_section)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:BakupSectionData (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                         
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (bakup_section_data)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:ImportSectionData (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                         
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (import_section_data)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)

(defun C:RSTRemoveSectionPoint (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any)                                         
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (remove_section_point)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)