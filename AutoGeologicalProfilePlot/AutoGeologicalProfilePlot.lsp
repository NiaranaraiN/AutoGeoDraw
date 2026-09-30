                         

                                 
	(if (null vlax-dump-object) (vl-load-com))
	(if (null vls-core-load) (load "TbxlsCore"))
	(if (null vls-text-load) (load "TbxlsText"))

              
(defun select_feature_point_type (self.dcl_id.int / __sfpt_set_all_key_value __sfpt_action_tile_to_function __sfpt_run_function_by_dialog_id __sfpt_load_menu_window __key_list.list __current_select_point_type.int __select_point_type_list.list __dialog_name.str __dcl_key_list.list __key.str __dcl_key.str)                                            
	(defun __sfpt_set_all_key_value ( / __dcl_key.str )                  
		(foreach __dcl_key.str __dcl_key_list.list
			(set (read (strcat "*" __dcl_key.str "_value*")) (get_tile __dcl_key.str))
		)
	)
  
	(defun __sfpt_action_tile_to_function (self.recv_key.str)                                                                       
		(cond
			((= self.recv_key.str "cancel")            
				(done_dialog 0)
			)
			((= self.recv_key.str "accept")            
				(done_dialog 1)
			)
			((= self.recv_key.str "help")            
				(__sfpt_set_all_key_value)
				(done_dialog 2)
			)
			((= self.recv_key.str "LimitsPoint")
				(__sfpt_set_all_key_value)
				(done_dialog 3)
			)
			((= self.recv_key.str "StructurePoint")
				(__sfpt_set_all_key_value)
				(done_dialog 4)
			)
		)
	)
	(defun __sfpt_run_function_by_dialog_id (self.dcl_id.int / __ss_length.int)                    
		(cond
			((= self.dcl_id.int 0)
       (start_main_menu self.dcl_id.int)
			)
			((= self.dcl_id.int 1)
       (start_main_menu self.dcl_id.int)
			)
			((= self.dcl_id.int 3)
       (setq __current_select_point_type.int 1)                     
       (setq __select_point_type_list.list (append __select_point_type_list.list (list __current_select_point_type.int)) )             
       (input_stratum_limits_point self.dcl_id.int)
			)
			((= self.dcl_id.int 4)
      	(setq __current_select_point_type.int 2)                     
				(setq __select_point_type_list.list (append __select_point_type_list.list (list __current_select_point_type.int)) )             
       (input_structure_point self.dcl_id.int)
			)
		)
	)
	(defun sfpt_send_global_value ()                                                  
		                                                  
	)
	(defun __sfpt_load_menu_window (self.dcl_id.int / )         
		(setq __dialog_name.str "SelectPointType")
		(setq __dcl_key_list.list '("LimitsPoint"  "StructurePoint"))
    (setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit))
		(if (not (new_dialog __dialog_name.str self.dcl_id.int)) (exit))
		(foreach __key.str __dcl_key_list.list
			(if (eval (read (strcat "*" __key.str "_value*")))
				(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
				(set (read (strcat "*" __key.str "_value*")) "NULL")
			)         
			(action_tile __key.str "(__sfpt_action_tile_to_function $key)")         
		)
		(setq *dialog_return_id* (start_dialog))
		(__sfpt_run_function_by_dialog_id *dialog_return_id*)
		(unload_dialog self.dcl_id.int)    
	)
  (__sfpt_load_menu_window self.dcl_id.int)
)
(defun input_stratum_limits_point ( self.dcl_id.int / __isl_key_value_get __isl_action_format_keys __isl_botton_action __isl_get_input_value.any __isl_load_menu __key_list.list __dialog_return_id.int __action_status.bool __key.str)                     
	(defun __isl_key_value_get ( / __temp_key.str )                  
		(foreach __key.str __key_list.list
			(set (read (strcat "*" __key.str "_value*")) (get_tile __key.str))
		)
	)
  (defun __isl_action_format_keys (self.key.str)                                                                       
		(cond
			((= self.key.str "cancel")            
				(done_dialog 0)                                              
			)
			((= self.key.str "accept")            
       	(__isl_key_value_get)
				                                   
			 	(done_dialog 1)
			)
    	((= self.key.str "location_name")            
        (__isl_key_value_get)
			 	                        
			)
    	((= self.key.str "soil_limits")            
        (__isl_key_value_get)
			 	                 
			)
			((= self.key.str "intense_limits")            
        (__isl_key_value_get)
			 	                 
			)
	    ((= self.key.str "moderate_limits")            
        (__isl_key_value_get)
			 	                 
			)
    )
	)
  (defun __isl_botton_action (self.isl_botton_id.int /)                    
    (cond
			( (= self.isl_botton_id.int 0) 
      	(start_main_menu self.dcl_id.int)
      )
			( (= self.isl_botton_id.int 1) 
				     
				(setq *location_name_list* (append *location_name_list* (list *location_name_value*)) )                      
				(setq *soil_limits_list* (append *soil_limits_list* (list *soil_limits_value*)) )                                    
				(setq *intense_limits_list* (append *intense_limits_list* (list *intense_limits_value*)) )                                
				(setq *moderate_limits_list* (append *moderate_limits_list* (list *moderate_limits_value*)) )                                
				(setq *limits_point_list* (append *limits_point_list* (list *get_point*)))                           
				       
				                                                                                 
				                                                                                  
				(start_main_menu self.dcl_id.int)
      )
		)
  )
                                                       
	                
	   
  (defun __isl_load_menu (self.dcl_id.int /  )          
    (setq __action_status.bool 2)
    (setq __key_list.list '("location_name" "soil_limits" "intense_limits" "moderate_limits"))
    (setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit))
		(if (not (new_dialog "InputLimit" self.dcl_id.int)) (exit))
    (while (> __action_status.bool 1)
			(foreach __key.str __key_list.list
				(if (eval (read (strcat "*" __key.str "_value*")))
					(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
					(set (read (strcat "*" __key.str "_value*")) "NULL")
				)         
				(if (or (= (length *soil_limits_list*) "NULL") (= (length *soil_limits_list*) 0))
						(setq *location_name_value* (strcat "pt" (itoa 1)))
						(setq *location_name_value* (strcat "pt" (itoa (1+ (length *soil_limits_list*)))))
        )
				(action_tile __key.str "(__isl_action_format_keys $key)")         
			)
			(setq __action_status.bool (start_dialog))
		)
		(unload_dialog self.dcl_id.int)
		(__isl_botton_action __action_status.bool)
  )
  
  (__isl_load_menu self.dcl_id.int)
)

(defun input_structure_point (self.dcl_id.int / __isp_set_all_key_value __isp_action_tile_to_function __isp_run_function_by_dialog_id __isp_load_menu_window __dcl_key_list.list __dialog_name.str __key.str __structure_point_type.int __dialog_status.bool __dcl_key.str)                    
	(defun __isp_set_all_key_value ( / __dcl_key.str )                  
		(foreach __dcl_key.str __dcl_key_list.list
			(set (read (strcat "*" __dcl_key.str "_value*")) (get_tile __dcl_key.str))
		)
	)
	(defun __isp_action_tile_to_function (self.recv_key.str)                                                                       
		(cond
			((= self.recv_key.str "cancel")            
				(done_dialog 0)
			)
			((= self.recv_key.str "accept")            
				(done_dialog 1)
			)
			((= self.recv_key.str "help")            
				(__isp_set_all_key_value)
				(done_dialog 2)
			)
			((= self.recv_key.str "IsBoundary")
				(__isp_set_all_key_value)
				(done_dialog 3)
			)
			((= self.recv_key.str "IsFault")
				(__isp_set_all_key_value)
				(done_dialog 4)
			)
			((= self.recv_key.str "IsFold")
				(__isp_set_all_key_value)
				(done_dialog 5)
			)
    	((= self.recv_key.str "StructurePointName")
				(__isp_set_all_key_value)
				                 
			)
			((= self.recv_key.str "LastGeoCode")
				(__isp_set_all_key_value)
				                 
			)
			((= self.recv_key.str "NextGeoCode")
				(__isp_set_all_key_value)
				                 
			)
		)
	)
	(defun __isp_run_function_by_dialog_id (self.recv_dialog_id.int / __ss_length.int)                    
		(cond
			((= self.recv_dialog_id.int 0)
      	(start_main_menu self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 1)
       "IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"
      	(setq *structure_point_type_list* (append *structure_point_type_list* (list __structure_point_type.int)) )
				(setq *structure_point_name_list* (append *structure_point_name_list* (list *structurepointname_value*)))
				(setq *last_geo_code_list* (append *last_geo_code_list* (list *lastgeocode_value*)))
				(setq *next_geo_code_list* (append *next_geo_code_list* (list *nextgeocode_value*)))
				(setq *structure_point_list* (append *structure_point_list* (list *get_point*)))
				                                                                                       
				                                                                                        
       
      	(start_main_menu self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 2)
      	(alert "Help")
			)
			((= self.recv_dialog_id.int 3)
      	(alert "地层分界线")
       	(setq __structure_point_type.int 3)
			)
    	((= self.recv_dialog_id.int 4)
      	(alert "断层")
       	(setq __structure_point_type.int 4)
			)
			((= self.recv_dialog_id.int 5)
      	(alert "向斜")
       	(setq __structure_point_type.int 5)
			)
    	((= self.recv_dialog_id.int 6)
       
			)
			((= self.recv_dialog_id.int 7)
       
			)
    	((= self.recv_dialog_id.int 8)
       
			)

		)
	)
	(defun isp_send_global_value ()                                                  
		                                                  
	)
	(defun __isp_load_menu_window (self.dcl_id.int / )         
		(setq __dialog_status.bool 2 __dialog_name.str "InputStructure")
		(setq __dcl_key_list.list '("IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"))
    (setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit)) 
		(while (> __dialog_status.bool 1)                                              
			(if (not (new_dialog __dialog_name.str self.dcl_id.int)) (exit))
			(foreach __key.str __dcl_key_list.list
				(if (eval (read (strcat "*" __key.str "_value*")))
					(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
					(set (read (strcat "*" __key.str "_value*")) "NULL")
				)         
				(if (or (= *structurepointname_value* "NULL") (= (length *structure_point_name_list*) 0))
					(setq *structurepointname_value* "pt1")
					(setq *structurepointname_value* (strcat "pt" (itoa (1+ (length *structure_point_name_list*)))))
				)
				(action_tile __key.str "(__isp_action_tile_to_function $key)")         
			)
			(setq __dialog_status.bool (start_dialog))
			(__isp_run_function_by_dialog_id __dialog_status.bool)
		)
		(unload_dialog self.dcl_id.int)    
	)
  (__isp_load_menu_window self.dcl_id.int)
)

(defun menu_confrim_end (self.dcl_id.int / __set_all_key_value.str __mce_action_tile_to_function __mce_run_function_by_dialog_id __mce_load_menu_window __dcl_key_list.list __dcl_id.int __dialog_name.str __key.str)               
	(defun __mce_action_tile_to_function (self.recv_key.str)                                                                       
		(cond
			((= self.recv_key.str "cancel")            
				(done_dialog 0)
			)
			((= self.recv_key.str "accept")            
       (done_dialog 1)
			)
		)
	)
	(defun __mce_run_function_by_dialog_id (self.recv_dialog_id.int / __ss_length.int)                    
		(cond
			((= self.recv_dialog_id.int 0)
       (setq *confirmendtext_value* "是否结束选点？")
       (start_main_menu __dcl_id.int)
			)
			((= self.recv_dialog_id.int 1)
				(setq *confirmendtext_value* "是否结束选点？")
			)
		)
	)
	(defun __mce_load_menu_window (self.dcl_id.int / )         
		(setq self.dcl_id.int 2 __dialog_name.str "ConfirmEnd")
    (setq __dcl_key_list.list '("ConfirmEndText"))
		(setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit))
		(if (not (new_dialog __dialog_name.str self.dcl_id.int)) (exit))
		(foreach __key.str __dcl_key_list.list
			(if (eval (read (strcat "*" __key.str "_value*")))
				(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
				(set (read (strcat "*" __key.str "_value*")) "是否结束选点？")
			)         
			(action_tile __key.str "(__mce_action_tile_to_function $key)")         
		)
		(setq self.dcl_id.int (start_dialog))
		(__mce_run_function_by_dialog_id self.dcl_id.int)
	)
  (unload_dialog __dcl_id.int)
  (__mce_load_menu_window __dcl_id.int)
)

(defun menu_confrim_cancel (self.dcl_id.int / __set_all_key_value.str __mcc_action_tile_to_function __mcc_run_function_by_dialog_id __mcc_load_menu_window __dcl_key_list.list __dialog_return_id.int __dialog_name.str __key.str)              
	(defun __mcc_action_tile_to_function (self.recv_key.str)                                                                       
		(cond
			((= self.recv_key.str "cancel")            
				(done_dialog 0)
			)
			((= self.recv_key.str "accept")            
				(done_dialog 1)
			)
		)
	)
	(defun __mcc_run_function_by_dialog_id (self.dcl_id.int /)                    
		(cond
			((= self.dcl_id.int 0)
      	(setq *confirmcanceltext_value* "确定终止程序？")
      	(start_main_menu self.dcl_id.int)
			)
			((= self.dcl_id.int 1)
      	(setq *confirmcancel_value* "确定终止程序？")
				(term_dialog)
			)
		)
	)
	(defun __mcc_load_menu_window (self.dcl_id.int / )         
		(setq __dialog_name.str "ConfirmCancel")
		(setq __dcl_key_list.list '("ConfirmCancelText"))
    (setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit))
		(if (not (new_dialog __dialog_name.str self.dcl_id.int)) (exit))
		(foreach __key.str __dcl_key_list.list
			(if (eval (read (strcat "*" __key.str "_value*")))
				(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
				(set (read (strcat "*" __key.str "_value*")) "确定终止程序？")
			)         
			(action_tile __key.str "(__mcc_action_tile_to_function $key)")         
		)
		(setq __dialog_return_id.int (start_dialog))
    (unload_dialog self.dcl_id.int)
		(__mcc_run_function_by_dialog_id __dialog_return_id.int)
	)
  (__mcc_load_menu_window self.dcl_id.int)
)

(defun normalize_saved_name (self.name.str)
  (cond
    ((= (strcase self.name.str) "IMPORT_FILE_PATH") "*import_file_path*")
    ((and (> (strlen self.name.str) 1)
          (= (substr self.name.str 1 1) "*")
          (/= (substr self.name.str (strlen self.name.str) 1) "*"))
     (strcat self.name.str "*"))
    (T self.name.str)
  )
)

(defun start_main_menu (self.dcl_id.int / __smm_set_all_key_value __smm_action_tile_to_function __smm_run_function_by_dialog_id __smm_load_menu_window __dcl_key_list.list __dialog_return_id.int __dialog_name.str __key.str __structure_point_type.int __ent_type.int __saved_raw_data.str __data_list.list __data_type.str __raw_data.file __saved_data_list.list __saved_data.any __dcl_key.str)         
	(defun __smm_set_all_key_value ( / __dcl_key.str )                  
		(foreach __dcl_key.str __dcl_key_list.list
			(set (read (strcat "*" __dcl_key.str "_value*")) (get_tile __dcl_key.str))
		)
	)
  
	(defun __smm_action_tile_to_function (self.recv_key.str)                                                                       
		(cond
			((= self.recv_key.str "cancel")            
				(done_dialog 0)
			)
			((= self.recv_key.str "accept")            
				(done_dialog 1)
			)
			((= self.recv_key.str "help")            
				(__smm_set_all_key_value)
				(done_dialog 2)
			)
			((= self.recv_key.str "AddFeaturePoint")
				(__smm_set_all_key_value)
				(done_dialog 3)
			)
			((= self.recv_key.str "RepickFeaturePoint")
				(__smm_set_all_key_value)
				(done_dialog 4)
			)
			((= self.recv_key.str "DeleteLastPoint")
				(done_dialog 5)
			)
			((= self.recv_key.str "SelectSectionLine")
				(done_dialog 6)
			)
			((= self.recv_key.str "isSelectSectionLine")                   
				(__smm_set_all_key_value)
				(done_dialog 7)
			)
			((= self.recv_key.str "SelectSectionGround")
				(done_dialog 8)
			)
			((= self.recv_key.str "isSelectSectionGround")                   
				(__smm_set_all_key_value)
				(done_dialog 9)
			)
			((= self.recv_key.str "PlanScale")             
				(__smm_set_all_key_value)
				(setq *plan_scale* (atof *planscale_value*))             
				                  
			)
			((= self.recv_key.str "SectionScale")             
				(__smm_set_all_key_value)
  			(setq *section_scale* (atof *sectionscale_value*))             
				                  
			)
			((= self.recv_key.str "ImportBakupData")           
				(__smm_set_all_key_value)
				                  
			)
			((= self.recv_key.str "BottonForImport")           
				(__smm_set_all_key_value)
				(done_dialog 13)
			)
			((= self.recv_key.str "OutputBakupData")           
				(__smm_set_all_key_value)
				                  
			)
			((= self.recv_key.str "BottonForOutput")           
				(__smm_set_all_key_value)
				(done_dialog 15)
			)
		)
	)
	(defun __smm_run_function_by_dialog_id (self.recv_dialog_id.int / __is_line.ename)                    
		(cond
			((= self.recv_dialog_id.int 0)
       (menu_confrim_cancel self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 1)
       (menu_confrim_end self.dcl_id.int)
			)
    	((= self.recv_dialog_id.int 3)
       (setvar "osmode" 14335)
       (setq *get_point* (getpoint "选择剖面点："))
       (setvar "osmode" 0)
       (select_feature_point_type self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 4)
       (setvar "osmode" 14335)
       (setq *get_point* (getpoint "选择剖面点："))
       (setvar "osmode" 0)
       (select_feature_point_type self.dcl_id.int)
       
			)
			((= self.recv_dialog_id.int 5)
       (deleta-last-point)
       (__smm_load_menu_window self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 6)
			)
			( (= self.recv_dialog_id.int 7)                     
				(setq *selectsectionline_value* "已选择")
       	(setq __is_line.ename 0)
      	(while (= __is_line.ename 0)
					(setq *plan_line_in_plan* (car (entsel "请选择平面图的剖面线：")))                          
      	 	(setq __ent_type.int (cdr (assoc 0 (entget *plan_line_in_plan*))))
         	(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
            (progn
							(setq __is_line.ename 1)
              (setvar "osmode" 14335)
  						(setq *plan_start_point* (getpoint "选择剖面起点："))
							(setq *plan_end_point* (getpoint "选择剖面终点："))
  						(setq *obj_plan_line_in_plan* (vlax-ename->vla-object *plan_line_in_plan*))
              (setq *plan_line_handle* (cdr (assoc 5 (entget *plan_line_in_plan*))))                   
  					  (setvar "osmode" 0)
						)
				 	)
				)
      	(__smm_load_menu_window self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 8)            
			)
			((= self.recv_dialog_id.int 9)                     
       	(setq *selectsectionground_value* "已选择")
				(setq __is_line.ename 0)
				(while (= __is_line.ename 0)
      		(setq *section_line_ground* (car (entsel "请选择剖面的剖面线：")))                                                    
      	 	(setq __ent_type.int (cdr (assoc 0 (entget *section_line_ground*))))
         	(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
						(progn
							(setq __is_line.ename 1)
              (setvar "osmode" 14335)
  						(setq *section_start_point* (getpoint "选择剖面起点："))
							(setq *section_end_point* (getpoint "选择剖面终点："))
  						(setq *obj_section_line_ground* (vlax-ename->vla-object *section_line_ground*))
							(setq *section_line_handle* (cdr (assoc 5 (entget *section_line_ground*))))                   
  					  (setvar "osmode" 0)          
						)
					)
        )
       	(setq *obj_section_line_ground* (vlax-ename->vla-object *section_line_ground*))
      	(__smm_load_menu_window self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 10)
			)
			((= self.recv_dialog_id.int 11)
			)
			((= self.recv_dialog_id.int 12)
			)
			((= self.recv_dialog_id.int 13)                    
				(defun smm_output_dcl_value (self.data.any self.data_type.str / __result.any)
					(cond
						((= self.data_type.str "LIST" )
						(if self.data.any
							(setq __result.any (nth (1- (length self.data.any)) self.data.any))
						))
						((= self.data_type.str "STR" )
						(if self.data.any
							(cond
							((= (type self.data.any) 'STR) (setq __result.any self.data.any))
							((= (type self.data.any) 'INT) (setq __result.any (itoa self.data.any)))
							((= (type self.data.any) 'REAL) (setq __result.any (rtos self.data.any)))
							)
						))
					)
				__result.any   
				)
				(progn            
					(foreach __saved_raw_data.str *test_bakup_data*
						(setq __data_list.list (vls-string->list __saved_raw_data.str ":"))
						(setq __data_type.str (nth 1 __data_list.list))
						(cond 
							((= __data_type.str "INT") (set (read (normalize_saved_name (nth 0 __data_list.list))) (atoi (nth 2 __data_list.list)) ))
							((= __data_type.str "REAL") (set (read (normalize_saved_name (nth 0 __data_list.list))) (atof (nth 2 __data_list.list)) ))
							((= __data_type.str "STR") (set (read (normalize_saved_name (nth 0 __data_list.list))) (nth 2 __data_list.list) ))
							((= __data_type.str "LIST") (set (read (normalize_saved_name (nth 0 __data_list.list))) (read (nth 2 __data_list.list)) ))
            ) 
	        )
					(PROGN
						(setq *location_name_value* (smm_output_dcl_value *location_name_list* "LIST"))
						(setq *soil_limits_value* (smm_output_dcl_value *soil_limits_list* "LIST"))
						(setq *intense_limits_value* (smm_output_dcl_value *intense_limits_list* "LIST"))
						(setq *moderate_limits_value* (smm_output_dcl_value *moderate_limits_list* "LIST"))
						(setq __structure_point_type.int (smm_output_dcl_value *structure_point_type_list* "LIST"))
						(setq *structurepointname_value* (smm_output_dcl_value *structure_point_name_list* "LIST"))
						(setq *lastgeocode_value* (smm_output_dcl_value *last_geo_code_list* "LIST"))
						(setq *nextgeocode_value* (smm_output_dcl_value *next_geo_code_list* "LIST"))
						(setq *nextgeocode_value* (smm_output_dcl_value *next_geo_code_list* "LIST"))
						(setq *planscale_value* (smm_output_dcl_value *plan_scale* "STR"))
						(setq *sectionscale_value* (smm_output_dcl_value *section_scale* "STR"))
						(if *plan_line_handle*              
							(progn
								(setq *plan_line_in_plan* (handent *plan_line_handle*))
         				(setq __ent_type.int (cdr (assoc 0 (entget *plan_line_in_plan*))) )
								(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
									(progn
										(setq *obj_plan_line_in_plan* (vlax-ename->vla-object *plan_line_in_plan*))
										(setq *selectsectionline_value* "已选择")
									)
								)
              )
						)
						(if *section_line_handle*              
							(progn
								(setq *section_line_ground* (handent *section_line_handle*))
								(setq __ent_type.int (cdr (assoc 0 (entget *section_line_ground*))) )
								(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
								(progn
									(setq *obj_plan_line_in_plan* (vlax-ename->vla-object *plan_line_in_plan*))
									(setq *selectsectionground_value* "已选择")
								)
								)
              )
						)
          )
					(if (= recover_status "OK")
						(alert "recovery ok")
					)
			 	)
			(__smm_load_menu_window self.dcl_id.int)
			)
			((= self.recv_dialog_id.int 14)
			)
			((= self.recv_dialog_id.int 15)                    
       (if (and *import_file_path* (!= *import_file_path* "NULL"))
				(princ)
        (setq *import_file_path* (getfiled "选择恢复数据" "" "vlsd1" 48) )
       )
       (setq __raw_data.file (open *import_file_path* "w"))
       (setq __saved_data_list.list '())
       (foreach __saved_data.any  *saved_variants*
					(if __saved_data.any
					(setq __saved_data_list.list 
						(append __saved_data_list.list
							(list (strcat (vl-princ-to-string __saved_data.any) ":" (vl-princ-to-string (type (read __saved_data.any))) ":" (vl-princ-to-string (eval (read __saved_data.any))))) )
					))
       )
       (__smm_load_menu_window self.dcl_id.int)
			)
		)
  )
	(defun smm_send_global_value ()                                                  
		                                                  
	)
	(defun __smm_load_menu_window (self.dcl_id.int / )         
		(setq __dialog_return_id.int 2 __dialog_name.str "StartMenu")
		(setq __dcl_key_list.list '("AddFeaturePoint" "DeleteLastPoint" "RepickFeaturePoint" "SelectSectionLine" "isSelectSectionLine" "SelectSectionGround" "isSelectSectionGround" "PlanScale" "SectionScale" "ImportBakupData" "BottonForImport" "OutputBakupData" "BottonForOutput"))
    (setq self.dcl_id.int (load_dialog *dcl_path*))
		(if (<= self.dcl_id.int 0)(exit))
		                                                                             
			(if (not (new_dialog __dialog_name.str self.dcl_id.int)) (exit))
			(foreach __key.str __dcl_key_list.list
				(if (eval (read (strcat "*" __key.str "_value*")))
					(set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
					(set (read (strcat "*" __key.str "_value*")) "NULL")
				)         
				(action_tile __key.str "(__smm_action_tile_to_function $key)")         
			)
			(setq __dialog_return_id.int (start_dialog))
			(unload_dialog self.dcl_id.int)
			(__smm_run_function_by_dialog_id __dialog_return_id.int)
		   
	)
  (__smm_load_menu_window self.dcl_id.int)
)


                 



                  
                                                                                                                 
                   
                                                                         
                                                   
              
                           
                                                                                                                                                         
                     
     
   


(defun get_start_point_coefficient (self.obj_selected_line.obj self.selected_point.pt / __result.any)                                       
  (setq __result.any (abs (- (vla-get-Length self.obj_selected_line.obj) (vlax-curve-getdistatpoint self.obj_selected_line.obj self.selected_point.pt) )))
  __result.any
)

(defun get_limits_index_lists ( / __temp_dist.real __temp_dist.real __temp_point.pt)                                                                                                           
	(foreach __temp_point.pt *limits_point_list*
		(setq __temp_dist.real (vlax-curve-getdistatpoint *obj_plan_line_in_plan* __temp_point.pt))
		(setq __temp_dist.real (abs (- __temp_dist.real *delta_start_point_dist*)) )
		(setq *limits_dist_list* (append *limits_dist_list* (list __temp_dist.real)))
	)                            
	(setq *limits_dist_index_list* (vl-sort-i *limits_dist_list* *sign_comparator*))             
  (setq *next_limits_index_list* (cdr *limits_dist_index_list*))
)
(defun make_polyline ( self.point_list.list / __vertices.list __result.any __dxf_90.any)                 
	(setq __dxf_90.any  (cons 90 (length self.point_list.list)))
  (setq __vertices.list (mapcar '(lambda (self.p.any) (cons 10 self.p.any)) self.point_list.list))
               
  (setq __result.any (entmakex
    (append
      '((0 . "LWPOLYLINE")            
        (100 . "AcDbEntity")
        (100 . "AcDbPolyline")
        __dxf_90.any                      
        (70 . 0))                                   
      __vertices.list
    )
  ))
  __result.any
)

(defun make_sectionwith_line (self.recv_x.real self.recv_y.real / __top_point.pt __bottom_point.pt __temp_line.ename)
	(setq __top_point.pt (append (list self.recv_x.real) (list (+ self.recv_y.real 5000)) ) __bottom_point.pt (append (list self.recv_x.real) (list (- self.recv_y.real 5000))) )
	(setq __temp_line.ename (entmakex
		(list
			(cons 0 "LINE")
			(cons 10 __top_point.pt)
			(cons 11 __bottom_point.pt)
		)
	))
  __temp_line.ename
)
(defun get_point_at_line (self.obj_pline.obj self.recv_x.real self.recv_line.ename / __obj_line.obj __obj_intersection_points.list __intersection_points.list )                         

	(setq __obj_line.obj (vlax-ename->vla-object self.recv_line.ename))
	(setq __obj_intersection_points.list (vlax-invoke
		self.obj_pline.obj
		'IntersectWith
		__obj_line.obj
		acExtendNone
	))
	(if __obj_intersection_points.list
		(setq __intersection_points.list (vlax-safearray->list __obj_intersection_points.list))
  	(progn
			(alert "fuction :{get_point_at_line}未找到交点。")
			(exit)
	))
	__intersection_points.list
)
(defun get_draw_limits_lists ( /  __first_index_number.int __second_index_number.int __first_soils.any __first_soil_list_str.list __soil_str.str __second_soils.any __second_soil_list_str.list __first_intense.any __second_intense.any __first_moderate.int __second_moderate.int __x_increase_dist.real __insert_number.int __x_increment.real __curr_soils_length.int __j.int __intense_increment.real __moderate_increment.int __temp_coor_x.real __section_start_x_coordinate.pt __curr_coor_x.real __curr_soil_limits.list __curr_intense.any __curr_moderate.int __curr_coor_y.real __total_x_increment.real __intersectionwith_line.ename __length_list.list __temp_coor_y.real __temp_move_point.pt __at_start_point_line.pt __curr_soil_limits_str.list)                     
                                                                                                                                                                                                                                                   
	(defun input_draw_line_value (self.recv_coor_x.real self.recv_coor_y.real self.recv_soil_limits.list self.recv_intense.any self.recv_moderate.int / __index_number.int )
		(setq __index_number.int  0)
		(while (< __index_number.int (length self.recv_soil_limits.list))           
			                                                     
			(setq self.recv_coor_y.real (- self.recv_coor_y.real (nth __index_number.int self.recv_soil_limits.list) ))
			(set (read (strcat "*draw_soil_line_" (itoa __index_number.int) "*")) (append (read (strcat "*draw_soil_line_" (itoa __index_number.int) "*")) (list self.recv_coor_x.real self.recv_coor_y.real 0)))               
		)
		(setq self.recv_coor_y.real (- self.recv_coor_y.real self.recv_intense.any))           
		(setq *draw_intense_line* (append *draw_intense_line* (list self.recv_coor_x.real self.recv_coor_y.real 0)))             
		(setq self.recv_coor_y.real (- self.recv_coor_y.real self.recv_moderate.int))           
		(setq *draw_moderate_line* (append *draw_moderate_line* (list self.recv_coor_x.real self.recv_coor_y.real 0)))             
	)
  (defun add_draw_limits_value (self.index_1st.int self.index_2nd.int / __second_soil_limits.list __first_soil_limits.list)
		(setq __first_index_number.int (nth self.index_1st.int *limits_dist_index_list*))
		(setq __second_index_number.int (nth self.index_2nd.int *next_limits_index_list*))
		(setq __first_soils.any  (nth __first_index_number.int *soil_limits_list*))                 
  	(setq __first_soil_list_str.list (vls-string->list __first_soils.any ","))         
    (setq __first_soil_limits.list '())
		(foreach __soil_str.str __first_soil_list_str.list
			(setq __first_soil_limits.list (append __first_soil_limits.list (list (atof __soil_str.str))))
    )                                           
		(setq __second_soils.any  (nth __second_index_number.int *soil_limits_list*))                   
    (setq __second_soil_list_str.list (vls-string->list __second_soils.any ","))         
    (setq __second_soil_limits.list '())
		(foreach __soil_str.str __second_soil_list_str.list
			(setq __second_soil_limits.list (append __second_soil_limits.list (list (atof __soil_str.str))))
    )                                            
		(setq __first_intense.any  (atof (nth __first_index_number.int *intense_limits_list*)))             
		(setq __second_intense.any  (atof (nth __second_index_number.int *intense_limits_list*)))             
		(setq __first_moderate.int  (atof (nth __first_index_number.int *moderate_limits_list*)))             
		(setq __second_moderate.int  (atof (nth __second_index_number.int *moderate_limits_list*)))             

		(setq __x_increase_dist.real  (* (atof (nth __first_index_number.int *limits_dist_list*)) *x_scale*) )                          
		(cond
			((> __x_increase_dist.real 2000) (setq __insert_number.int (1- (/ (fix __x_increase_dist.real) 100))) )
			((> __x_increase_dist.real 1000) (setq __insert_number.int (1- (/ (fix __x_increase_dist.real) 30 ))) )
			((> __x_increase_dist.real 100 ) (setq __insert_number.int (1- (/ (fix __x_increase_dist.real) 15 ))) )
			((> __x_increase_dist.real 10  ) (setq __insert_number.int (1- (/ (fix __x_increase_dist.real) 5  ))) )
			(T (setq __insert_number.int 2) )
    )                                          
		(setq __x_increment.real (/ __x_increase_dist.real __insert_number.int))                       
		(setq __curr_soils_length.int (length __second_soil_limits.list))
	
    (setq __j.int  0)
  	(while (< __j.int __curr_soils_length.int)
			(set (read (strcat "*first_soil_" (itoa __j.int) "*")) (nth __j.int __first_soil_limits.list) )
			(set (read (strcat "*second_soil_" (itoa __j.int) "*")) (nth __j.int __second_soil_limits.list) )
      (set (read (strcat "*soil_increment_" (itoa __j.int)  "*")) (/ (* (- (nth __j.int __second_soil_limits.list) (nth __j.int __first_soil_limits.list)) *y_scale*) (1+ *y_scale*)) )
			(setq __j.int  (1+ __j.int))
    )                                              
  	(setq __intense_increment.real (/ (* (- __second_intense.any __first_intense.any) *y_scale*) (1+ __insert_number.int)) )                              
  	(setq __moderate_increment.int (/ (* (- __second_moderate.int __first_moderate.int) *y_scale*) (1+ __insert_number.int)) )                                 
		(setq __temp_coor_x.real __section_start_x_coordinate.pt)                
		(setq __section_start_x_coordinate.pt (+ __section_start_x_coordinate.pt __x_increase_dist.real))                                             
		
		(setq __curr_coor_x.real __temp_coor_x.real)        
		(setq __curr_soil_limits.list __first_soil_limits.list)
		(setq __curr_intense.any __first_intense.any)
   	(setq __curr_moderate.int __second_moderate.int)
		(while (< __curr_coor_x.real __section_start_x_coordinate.pt)            
			(setq __curr_coor_y.real (cadr (get_point_at_line *obj_section_line_ground* __curr_coor_x.real __intersectionwith_line.ename)) )                   
			(input_draw_line_value __curr_coor_x.real __curr_coor_y.real __curr_soil_limits.list __curr_intense.any __curr_moderate.int)
               
			              
			                                                    
			                                                         
			                                                                
			                                                                                                                                                          
			     
			                                                             
			                                                                                          
			                                                              
			                                                                                            
     
			(vl-cmdf "move" __intersectionwith_line.ename "" __temp_move_point.pt  __x_increment.real)
			(setq __curr_coor_x.real (+ __x_increment.real __curr_coor_x.real))        
		)
		                                 

   	(entdel __intersectionwith_line.ename)
		(setq __total_x_increment.real (+ __total_x_increment.real __x_increase_dist.real))
		(vl-cmdf "copy" __at_start_point_line.pt "" 0 __total_x_increment.real)
		(setq __intersectionwith_line.ename (entlast))
		(setq *i* (1+ *i*))
	)
  (defun start_get_draw_limits_lists ( / )
		(setq __length_list.list (length *next_limits_index_list*) *i* 0)
		(setq __temp_coor_y.real (cadr *section_start_point*))                        
		(setq __section_start_x_coordinate.pt (car *section_start_point*))
		(setq __total_x_increment.real 0)                                                                
		(setq __temp_move_point.pt (list __section_start_x_coordinate.pt __temp_coor_y.real))         
		(setq __at_start_point_line.pt (make_sectionwith_line __section_start_x_coordinate.pt __temp_coor_y.real))                    
		(vl-cmdf "copy" __at_start_point_line.pt "" 0 0)
		(setq __intersectionwith_line.ename (entlast))                             
		               
		(while (< *i* __length_list.list)
			             
				                                                           
				                                                            
				                                                                                 
				                                                                    
				                                   
				                                                                             
				                                              
				                                                                                     
				                                                                  
				                                    
				                                                                               
				                                               
				                                                                                         
				                                                                                           
				                                                                                           
				                                                                                             

				                                                                                                                 
				       
				                                                                                      
				                                                                                      
				                                                                                      
				                                                                                      
				                              
				                                             
				                                                                             
				                                                      
				
			
				             
				                                
				                                                                          
				                                                                            
				                                                                                                                                            
				                   
				                                                 
				                                                                                                                             
				                                                                                                                                   
				                                                               
				                                                                                                                               
				
				                                        
				                                           
				                                   
				                                      
				                                                               
				                                                                                                                               
				              
				                                                    
				                                                         
				                                                                
				                                                                                                                                                          
				     
				                                                             
				                                                                                          
				                                                              
				                                                                                            

				                                                                         
				                                                         
				   
				                                   
				                                
				                                                                
				                                                             
				                                        
			 
			(add_draw_limits_value *i* *i*)
			(setq *i* (1+ *i*))
		)
		                   
		(progn                    
			(setq __curr_coor_x.real (car *section_end_point*  ))        
			(setq __curr_coor_y.real (cadr *section_end_point*))        
			(setq __first_soils.any  (nth *i* *soil_limits_list*))                 
			(setq __curr_soil_limits_str.list (vls-string->list __first_soils.any ","))         
			(setq __curr_soil_limits.list '())
			(foreach __soil_str.str __curr_soil_limits_str.list
					(setq __curr_soil_limits.list (append __curr_soil_limits.list (list (atof __soil_str.str))))
			)
			(setq *soil_count* (length __curr_soil_limits.list))
			(setq __curr_intense.any (atof (nth *i* *intense_limits_list*)))
			(setq __curr_moderate.int (atof (nth *i* *intense_limits_list*)))
			(input_draw_line_value __curr_coor_x.real __curr_coor_y.real __curr_soil_limits.list __curr_intense.any __curr_moderate.int)
		)
	)
  (start_get_draw_limits_lists)
)
(defun draw_soil_line ()
	(get_draw_limits_lists)
	(setq *i* 0)
  (while (< *i* *soil_count*)
    (make_polyline (read (strcat "*draw_soil_line_" (itoa *i*) "*")))
		(1+ *i*)
  )
	(make_polyline  *draw_intense_line*)
	(make_polyline  *draw_intense_line*)
)



(defun init_variant ( / __structure_point_type.int)
  (setq *dcl_path* "W:\\Plugin\\LISP\\AutoGeoDraw\\AutoGeologicalProfilePlot\\AGPPMenu.dcl")
  (setq *dcl_id* 999)
  (setq *saved_variants* '("*plan_start_point*" "*plan_end_point*" "*plan_line_handle*" "*section_start_point*" "*section_end_point*" "*section_line_handle*" "*location_name_list*" "*soil_limits_list*" "*intense_limits_list*" "*moderate_limits_list*" "*limits_point_list*" "*structure_point_type_list*" "*structure_point_name_list*" "*last_geo_code_list*" "*next_geo_code_list*" "*structure_point_list*" "*plan_scale*" "*section_scale*" "*data_saved_path*" "*data_import_path*" "*import_file_path*"))
  (setq *import_file_path* '())
  (setq *output_file_path* '())
              
		                                               
		                                                         
		                                                                    
		                                                
		                                                   
		                                                    
		                                                     
		                                              
		                                                
		                                                
		
		                                                                       
		                                                                                   
		(setq __structure_point_type.int 3)                               
		                                                       
		                                                     
		                                                      
		                                                      
		                                                    
		                                                      
		                                                      
		                                                                                                  
		                                                  
		                                                          
		                                                  
		                                                          
		                                                 
		                                               
		                                                    
		                                                  
   
)


(defun set_variant ( /  __section_start_x_coordinate.pt)
  (setq *plan_scale* (atof *planscale_value*))             
  (setq *section_scale* (atof *sectionscale_value*))             
  (setq *x_scale* (/ *plan_scale* *section_scale*))         
  (setq *y_scale* (/ 1000 *section_scale*))         
	(setq *delta_start_point_dist* (get_start_point_coefficient *obj_plan_line_in_plan* *plan_start_point*))                                                 
  (setq __section_start_x_coordinate.pt (car *section_start_point*))
  (if (> *delta_start_point_dist* 0)                      
		(setq *sign_comparator* (quote >))
		(setq *sign_comparator* (quote <))
	)
	(get_limits_index_lists)                                            
  (get_draw_limits_lists) 
)

                                                          
                               
   

(defun print_variant (self.variant_list.list / __i.int  __list_length.int __var.any)
  (setq __list_length.int (length self.variant_list.list))
  (foreach __var.any self.variant_list.list)
	(princ (strcat (read __var.any) "is:" __var.any "\n"))
  (princ)
)



                  
                                                                                                                             
                                                                                                         
   

                          
(defun start_program ( / )
  (setq *test_bakup_data* '("*plan_line_handle*:STR:2CEE" "*section_line_handle*:STR:2CF0" "*location_name_list*:LIST:(pt1 pt2 pt2 11 12)" "*soil_limits_list*:LIST:(3,4,5 3,4,6 3,4,3 3,4,3 3,4,3)" "*intense_limits_list*:LIST:(2 3 4 4 4)" "*moderate_limits_list*:LIST:(4 7 5 5 5)" "*limits_point_list*:LIST:((307.961 1647.39 0.0) (509.677 1849.11 0.0) (942.09 2281.52 0.0) (1194.5 2533.93 0.0) (1418.96 2758.39 0.0))" "*structure_point_type_list*:LIST:(3 4 5)" "*structure_point_name_list*:LIST:(pt1 pt2 4)" "*last_geo_code_list*:LIST:(1 3 5)" "*next_geo_code_list*:LIST:(2 4 6)" "*structure_point_list*:LIST:((523.755 1863.19 0.0) (736.945 2076.38 0.0) (1181.43 2520.86 0.0))" "*plan_scale*:STR:50000.0" "*section_scale*:STR:500.0" "*data_saved_path:STR:nil" "*data_import_path:STR:nil" "recover_status:STR:OK"))
	(init_variant)
	(start_main_menu *dcl_id*)
                               
	(set_variant)
                                
  (alert "start_drawing")
  (draw_soil_line)
                       
                          
                         
                 
          
  
  
  (print_variant)
)


(defun c:test( /  __structure_point_type.int __ent_type.int __saved_raw_data.str __data_list.list __data_type.str)
  (alert "vmodify: 2024-09-13 00:00")
                                  
                                             
			                          
			                            
                                                                                                                                                                    
			     
      (defun smm_output_dcl_value (self.data.any self.data_type.str / __result.any)
        (cond
					((= self.data_type.str "LIST" )
					(if self.data.any
						(setq __result.any (nth (1- (length self.data.any)) self.data.any))
					))
					((= self.data_type.str "STR" )
					(if self.data.any
						(cond
           	((= (type self.data.any) 'STR) (setq __result.any self.data.any))
           	((= (type self.data.any) 'INT) (setq __result.any (itoa self.data.any)))
           	((= (type self.data.any) 'REAL) (setq __result.any (rtos self.data.any)))
						)
					))
				)
       __result.any   
			)
				(progn            
      		(setq *test_bakup_data* '("*plan_line_handle*:STR:2CEE" "*section_line_handle*:STR:2CF0" "*location_name_list*:LIST:(pt1 pt2 pt2 11 12)" "*soil_limits_list*:LIST:(3,4,5 3,4,6 3,4,3 3,4,3 3,4,3)" "*intense_limits_list*:LIST:(2 3 4 4 4)" "*moderate_limits_list*:LIST:(4 7 5 5 5)" "*limits_point_list*:LIST:((307.961 1647.39 0.0) (509.677 1849.11 0.0) (942.09 2281.52 0.0) (1194.5 2533.93 0.0) (1418.96 2758.39 0.0))" "*structure_point_type_list*:LIST:(nil 4 5)" "*structure_point_name_list*:LIST:(pt1 pt2 4)" "*last_geo_code_list*:LIST:(1 3 5)" "*next_geo_code_list*:LIST:(2 4 6)" "*structure_point_list*:LIST:((523.755 1863.19 0.0) (736.945 2076.38 0.0) (1181.43 2520.86 0.0))" "*plan_scale*:STR:50000.0" "*section_scale*:STR:500.0" "*data_saved_path:STR:nil" "*data_import_path:STR:nil"))
					(foreach __saved_raw_data.str *test_bakup_data*
						(setq __data_list.list (vls-string->list __saved_raw_data.str ":"))
						(setq __data_type.str (nth 1 __data_list.list))
						(cond 
							((= __data_type.str "INT") (set (read (normalize_saved_name (nth 0 __data_list.list))) (atoi (nth 2 __data_list.list)) ))
							((= __data_type.str "REAL") (set (read (normalize_saved_name (nth 0 __data_list.list))) (atof (nth 2 __data_list.list)) ))
							((= __data_type.str "STR") (set (read (normalize_saved_name (nth 0 __data_list.list))) (nth 2 __data_list.list) ))
							((= __data_type.str "LIST") (set (read (normalize_saved_name (nth 0 __data_list.list))) (read (nth 2 __data_list.list)) ))
            )
                   
          
          
          )
					(PROGN
						(setq *location_name_value* (smm_output_dcl_value *location_name_list* "LIST"))
						(setq *soil_limits_value* (smm_output_dcl_value *soil_limits_list* "LIST"))
						(setq *intense_limits_value* (smm_output_dcl_value *intense_limits_list* "LIST"))
						(setq *moderate_limits_value* (smm_output_dcl_value *moderate_limits_list* "LIST"))
						(setq __structure_point_type.int (smm_output_dcl_value *structure_point_type_list* "LIST"))
						(setq *structurepointname_value* (smm_output_dcl_value *structure_point_name_list* "LIST"))
						(setq *lastgeocode_value* (smm_output_dcl_value *last_geo_code_list* "LIST"))
						(setq *nextgeocode_value* (smm_output_dcl_value *next_geo_code_list* "LIST"))
						(setq *nextgeocode_value* (smm_output_dcl_value *next_geo_code_list* "LIST"))
						(setq *planscale_value* (smm_output_dcl_value *plan_scale* "STR"))
						(setq *sectionscale_value* (smm_output_dcl_value *section_scale* "STR"))
						(if *plan_line_handle*              
							(progn
								(setq *plan_line_in_plan* (handent *plan_line_handle*))
         				(setq __ent_type.int (cdr (assoc 0 (entget *plan_line_in_plan*))) )
								(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
									(setq *obj_plan_line_in_plan* (vlax-ename->vla-object *plan_line_in_plan*))
									(setq *selectsectionline_value* "已选择")
								)
              )
						)
						(if *section_line_handle*              
							(progn
								(setq *section_line_ground* (handent *section_line_handle*))
								(setq __ent_type.int (cdr (assoc 0 (entget *section_line_ground*))) )
								(if (or (equal __ent_type.int "LWPOLYLINE") (equal __ent_type.int "LINE"))
									(setq *obj_plan_line_in_plan* (vlax-ename->vla-object *plan_line_in_plan*))
									(setq *selectsectionground_value* "已选择")
								)
              )
						)
          )
			 	)
					        
					                                                                                             
					                                                                                             
					                                                 
					                                                  
					                                                   
					   
					                       
					                           
					         
					                                                                 
					   
    		   
  
  
  
	                
	                          
  
                                               
                           
                                        
	              
	        
	                                          
	                 
	                    
	                             
                               
	               
                                
)










         
(defun C:TT (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __acadobj.obj __doc.obj __xx.any)                
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
  (setq __acadobj.obj (vlax-get-acad-object)
          __doc.obj (vla-get-ActiveDocument __acadobj.obj))
	(start_program)
	(command "redraw")         
	(if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
	(vla-endundomark __cur_doc.obj)          
	(princ)
)