; geological_profile_plot

;--------------------------------
	(if (null vlax-dump-object) (vl-load-com))
	(if (null vls-core-load) (load "TbxlsCore"))
	(if (null vls-text-load) (load "TbxlsText"))

;-----dcl-----
(defun select-feature-point-type (dcl_id / sfpt-set-all-key-value sfpt-action-tile->function sfpt-run-function-by-dialog-id sfpt-load-menu-window key_list); 选择特征点类型，是土层厚度点还是地质构造点
	(defun sfpt-set-all-key-value ( / dcl_key ) ;保存已经输入的值
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
  
	(defun sfpt-action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
				(done_dialog 1)
			)
			((= recv_key "help") ;{帮助按钮}
				(sfpt-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "LimitsPoint")
				(sfpt-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "StructurePoint")
				(sfpt-set-all-key-value)
				(done_dialog 4)
			)
		)
	)
	(defun sfpt-run-function-by-dialog-id (dcl_id / ss_length) ;按钮对应的具体操作
		(cond
			((= dcl_id 0)
       (start-main-menu dcl_id)
			)
			((= dcl_id 1)
       (start-main-menu dcl_id)
			)
			((= dcl_id 3)
       (setq current_select_point_type 1);类型为土层厚度特征点
       (setq select_point_type_list (append select_point_type_list (list current_select_point_type)) );加入类型列表
       (input-stratum-limits-point dcl_id)
			)
			((= dcl_id 4)
      	(setq current_select_point_type 2);类型为地质构造特征点
				(setq select_point_type_list (append select_point_type_list (list current_select_point_type)) );加入类型列表
       (input-structure-point dcl_id)
			)
		)
	)
	(defun sfpt-send-global-value () ;特殊情况下，把控件对应的值传递到非局部变量参数；
		;(setq draw_scale  (read drawingScaleInput_value))
	)
	(defun sfpt-load-menu-window (dcl_id / );加载菜单
		(setq dialog_name "SelectPointType")
		(setq dcl_key_list '("LimitsPoint"  "StructurePoint"))
    (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "NULL")
			);控件内容
			(action_tile key "(sfpt-action-tile->function $key)");点击动作
		)
		(setq dialog_return_id (start_dialog))
		(sfpt-run-function-by-dialog-id dialog_return_id)
		(unload_dialog dcl_id)    
	)
  (sfpt-load-menu-window dcl_id)
)
(defun input-stratum-limits-point ( dcl_id / isl-key-value-get isl-action-format-keys isl-botton-action isl-get-input-value isl-load-menu key_list dialog_return_id);输入土层厚度特征点值
	(defun isl-key-value-get ( / temp_key ) ;保存已经输入的值
		(foreach key key_list
			(set (read (strcat key "_value")) (get_tile key))
		)
	)
  (defun isl-action-format-keys (key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= key "cancel") ;{取消按钮}
				(done_dialog 0);对话框退出返回主函数 传递给Dialog_Return值为0
			)
			((= key "accept") ;{确认按钮}
       	(isl-key-value-get)
				; (save-stratum-limits-point-value)
			 	(done_dialog 1)
			)
    	((= key "location_name") ;{确认按钮}
        (isl-key-value-get)
			 	; (done_dialog 3)       
			)
    	((= key "soil_limits") ;{确认按钮}
        (isl-key-value-get)
			 	; (done_dialog 4)
			)
			((= key "intense_limits") ;{确认按钮}
        (isl-key-value-get)
			 	; (done_dialog 5)
			)
	    ((= key "moderate_limits") ;{确认按钮}
        (isl-key-value-get)
			 	; (done_dialog 6)
			)
    )
	)
  (defun isl-botton-action (isl_botton_id /) ;按钮对应的具体操作
    (cond
			( (= isl_botton_id 0);
      	(start-main-menu dcl_id)
      )
			( (= isl_botton_id 1);
				;赋值
				(setq *location_name_list (append *location_name_list (list location_name_value)) );土厚度名称-->名称列表
				(setq *soil_limits_list (append *soil_limits_list (list soil_limits_value)) );加入土层列表-->土厚度（字符串）列表
				(setq *intense_limits_list (append *intense_limits_list (list intense_limits_value)) );加入强风化列表-->强风化厚度列表
				(setq *moderate_limits_list (append *moderate_limits_list (list moderate_limits_value)) );加信弱风化列表-->弱风化厚度列表
				(setq *limits_point_list (append *limits_point_list (list *get_point)));平面图上的点位-->点位列表=
				;备份值
				; (setq limits_point_x_list (append limits_point_x_list (list (car *get_point))))
				; (setq limits_point_y_list (append limits_point_y_list (list (cadr *get_point))))
				(start-main-menu dcl_id)
      )
		)
  )
  ; (defun isl-get-input-value () ;取得edit_box里的值；
	; 	(princ "str")
	; )
  (defun isl-load-menu (dcl_id /  );加载菜单 
    (setq action_status 2)
    (setq key_list '("location_name" "soil_limits" "intense_limits" "moderate_limits"))
    (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog "InputLimit" dcl_id)) (exit))
    (while (> action_status 1)
			(foreach key key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				);控件内容
				(if (or (= (length *soil_limits_list) "NULL") (= (length *soil_limits_list) 0))
						(setq location_name_value (strcat "pt" (itoa 1)))
						(setq location_name_value (strcat "pt" (itoa (1+ (length *soil_limits_list)))))
        )
				(action_tile key "(isl-action-format-keys $key)");点击动作
			)
			(setq action_status (start_dialog))
		)
		(unload_dialog dcl_id)
		(isl-botton-action action_status)
  )
  
  (isl-load-menu dcl_id)
)

(defun input-structure-point (dcl_id / isp-set-all-key-value isp-action-tile->function isp-run-function-by-dialog-id isp-load-menu-window dcl_key_list); 输入地质线特征点值
	(defun isp-set-all-key-value ( / dcl_key ) ;保存已经输入的值
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
	(defun isp-action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
				(done_dialog 1)
			)
			((= recv_key "help") ;{帮助按钮}
				(isp-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "IsBoundary")
				(isp-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "IsFault")
				(isp-set-all-key-value)
				(done_dialog 4)
			)
			((= recv_key "IsFold")
				(isp-set-all-key-value)
				(done_dialog 5)
			)
    	((= recv_key "StructurePointName")
				(isp-set-all-key-value)
				; (done_dialog 6)
			)
			((= recv_key "LastGeoCode")
				(isp-set-all-key-value)
				; (done_dialog 7)
			)
			((= recv_key "NextGeoCode")
				(isp-set-all-key-value)
				; (done_dialog 8)
			)
		)
	)
	(defun isp-run-function-by-dialog-id (recv_dialog_id / ss_length) ;按钮对应的具体操作
		(cond
			((= recv_dialog_id 0)
      	(start-main-menu dcl_id)
			)
			((= recv_dialog_id 1)
       "IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"
      	(setq *structure_point_type_list (append *structure_point_type_list (list structure_point_type)) )
				(setq *structure_point_name_list (append *structure_point_name_list (list StructurePointName_value)))
				(setq *last_geo_code_list (append *last_geo_code_list (list LastGeoCode_value)))
				(setq *next_geo_code_list (append *next_geo_code_list (list NextGeoCode_value)))
				(setq *structure_point_list (append *structure_point_list (list *get_point)))
				; (setq structure_point_x_list (append structure_point_x_list (list (car *get_point))))
				; (setq structure_point_y_list (append structure_point_y_list (list (cadr *get_point))))
       
      	(start-main-menu dcl_id)
			)
			((= recv_dialog_id 2)
      	(alert "Help")
			)
			((= recv_dialog_id 3)
      	(alert "地层分界线")
       	(setq structure_point_type 3)
			)
    	((= recv_dialog_id 4)
      	(alert "断层")
       	(setq structure_point_type 4)
			)
			((= recv_dialog_id 5)
      	(alert "向斜")
       	(setq structure_point_type 5)
			)
    	((= recv_dialog_id 6)
       
			)
			((= recv_dialog_id 7)
       
			)
    	((= recv_dialog_id 8)
       
			)

		)
	)
	(defun isp-send-global-value () ;特殊情况下，把控件对应的值传递到非局部变量参数；
		;(setq draw_scale  (read drawingScaleInput_value))
	)
	(defun isp-load-menu-window (dcl_id / );加载菜单
		(setq dialog_status 2 dialog_name "InputStructure")
		(setq dcl_key_list '("IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"))
    (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit)) 
		(while (> dialog_status 1) ; dialogStatus 为1时，是按了确定，0是按了取消
			(if (not (new_dialog dialog_name dcl_id)) (exit))
			(foreach key dcl_key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				);控件内容
				(if (or (= StructurePointName_value "NULL") (= (length *structure_point_name_list) 0))
					(setq StructurePointName_value "pt1")
					(setq StructurePointName_value (strcat "pt" (itoa (1+ (length *structure_point_name_list)))))
				)
				(action_tile key "(isp-action-tile->function $key)");点击动作
			)
			(setq dialog_status (start_dialog))
			(isp-run-function-by-dialog-id dialog_status)
		)
		(unload_dialog dcl_id)    
	)
  (isp-load-menu-window dcl_id)
)

(defun menu-confrim-end (dcl_id / set-all-key-value mce-action-tile->function mce-run-function-by-dialog-id mce-load-menu-window dcl_key_list dcl_id); 确认输入结束 
	(defun mce-action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
       (done_dialog 1)
			)
		)
	)
	(defun mce-run-function-by-dialog-id (recv_dialog_id / ss_length) ;按钮对应的具体操作
		(cond
			((= recv_dialog_id 0)
       (setq ConfirmEndText_value "是否结束选点？")
       (start-main-menu dcl_id)
			)
			((= recv_dialog_id 1)
				(setq ConfirmEndText_value "是否结束选点？")
			)
		)
	)
	(defun mce-load-menu-window (dcl_id / );加载菜单
		(setq dcl_id 2 dialog_name "ConfirmEnd")
    (setq dcl_key_list '("ConfirmEndText"))
		(setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "是否结束选点？")
			);控件内容
			(action_tile key "(mce-action-tile->function $key)");点击动作
		)
		(setq dcl_id (start_dialog))
		(mce-run-function-by-dialog-id dcl_id)
	)
  (unload_dialog dcl_id)
  (mce-load-menu-window dcl_id)
)

(defun menu-confrim-cancel (dcl_id / set-all-key-value mcc-action-tile->function mcc-run-function-by-dialog-id mcc-load-menu-window dcl_key_list dialog_return_id); 确认取消控件
	(defun mcc-action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
				(done_dialog 1)
			)
		)
	)
	(defun mcc-run-function-by-dialog-id (dcl_id /) ;按钮对应的具体操作
		(cond
			((= dcl_id 0)
      	(setq ConfirmCancelText_value "确定终止程序？")
      	(start-main-menu dcl_id)
			)
			((= dcl_id 1)
      	(setq ConfirmCancel_value "确定终止程序？")
				(term_dialog)
			)
		)
	)
	(defun mcc-load-menu-window (dcl_id / );加载菜单
		(setq dialog_name "ConfirmCancel")
		(setq dcl_key_list '("ConfirmCancelText"))
    (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "确定终止程序？")
			);控件内容
			(action_tile key "(mcc-action-tile->function $key)");点击动作
		)
		(setq dialog_return_id (start_dialog))
    (unload_dialog dcl_id)
		(mcc-run-function-by-dialog-id dialog_return_id)
	)
  (mcc-load-menu-window dcl_id)
)

(defun start-main-menu (dcl_id / smm-set-all-key-value smm-action-tile->function smm-run-function-by-dialog-id smm-load-menu-window dcl_key_list dialog_return_id); DCL模版
	(defun smm-set-all-key-value ( / dcl_key ) ;保存已经输入的值
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
  
	(defun smm-action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
				(done_dialog 1)
			)
			((= recv_key "help") ;{帮助按钮}
				(smm-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "AddFeaturePoint")
				(smm-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "RepickFeaturePoint")
				(smm-set-all-key-value)
				(done_dialog 4)
			)
			((= recv_key "DeleteLastPoint")
				(done_dialog 5)
			)
			((= recv_key "SelectSectionLine")
				(done_dialog 6)
			)
			((= recv_key "isSelectSectionLine") ; 选择剖面线的按钮
				(smm-set-all-key-value)
				(done_dialog 7)
			)
			((= recv_key "SelectSectionGround")
				(done_dialog 8)
			)
			((= recv_key "isSelectSectionGround") ; 选择剖面线的按钮
				(smm-set-all-key-value)
				(done_dialog 9)
			)
			((= recv_key "PlanScale") ; 平面图比例
				(smm-set-all-key-value)
				(setq *plan_scale (atof PlanScale_value));平面图的比例
				; (done_dialog 10)
			)
			((= recv_key "SectionScale") ; 剖面图比例
				(smm-set-all-key-value)
  			(setq *section_scale (atof SectionScale_value));剖面图的比例
				; (done_dialog 11)
			)
			((= recv_key "ImportBakupData") ; 导入路径
				(smm-set-all-key-value)
				; (done_dialog 12)
			)
			((= recv_key "BottonForImport") ; 导入按钮
				(smm-set-all-key-value)
				(done_dialog 13)
			)
			((= recv_key "OutputBakupData") ; 导出路径
				(smm-set-all-key-value)
				; (done_dialog 14)
			)
			((= recv_key "BottonForOutput") ; 导出按键
				(smm-set-all-key-value)
				(done_dialog 15)
			)
		)
	)
	(defun smm-run-function-by-dialog-id (recv_dialog_id / is_line) ;按钮对应的具体操作
		(cond
			((= recv_dialog_id 0)
       (menu-confrim-cancel dcl_id)
			)
			((= recv_dialog_id 1)
       (menu-confrim-end dcl_id)
			)
    	((= recv_dialog_id 3)
       (setvar "osmode" 14335)
       (setq *get_point (getpoint "选择剖面点："))
       (setvar "osmode" 0)
       (select-feature-point-type dcl_id)
			)
			((= recv_dialog_id 4)
       (setvar "osmode" 14335)
       (setq *get_point (getpoint "选择剖面点："))
       (setvar "osmode" 0)
       (select-feature-point-type dcl_id)
       
			)
			((= recv_dialog_id 5)
       (deleta-last-point)
       (smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 6)
			)
			( (= recv_dialog_id 7);选择平面图上的剖面线
				(setq SelectSectionLine_value "已选择")
       	(setq is_line 0)
      	(while (= is_line 0)
					(setq *plan_line_in_plan (car (entsel "请选择平面图的剖面线："))) ;得到平面图上的剖面线对象
      	 	(setq ent_type (cdr (assoc 0 (entget *plan_line_in_plan))))
         	(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
            (progn
							(setq is_line 1)
              (setvar "osmode" 14335)
  						(setq *plan_start_point (getpoint "选择剖面起点："))
							(setq *plan_end_point (getpoint "选择剖面终点："))
  						(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
              (setq *plan_line_handle (cdr (assoc 5 (entget *plan_line_in_plan))));备份线段的handle值
  					  (setvar "osmode" 0)
						)
				 	)
				)
      	(smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 8);选择7的按钮
			)
			((= recv_dialog_id 9);选择剖面图上的剖面线
       	(setq SelectSectionGround_value "已选择")
				(setq is_line 0)
				(while (= is_line 0)
      		(setq *section_line_ground (car (entsel "请选择剖面的剖面线："))) ;得到平面图上的剖面线对象 ;得到平面图上的剖面线对象
      	 	(setq ent_type (cdr (assoc 0 (entget *section_line_ground))))
         	(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
						(progn
							(setq is_line 1)
              (setvar "osmode" 14335)
  						(setq *section_start_point (getpoint "选择剖面起点："))
							(setq *section_end_point (getpoint "选择剖面终点："))
  						(setq *obj_section_line_ground (vlax-ename->vla-object *section_line_ground))
							(setq *section_line_handle (cdr (assoc 5 (entget *section_line_ground))));备份线段的handle值
  					  (setvar "osmode" 0)          
						)
					)
        )
       	(setq *obj_section_line_ground (vlax-ename->vla-object *section_line_ground))
      	(smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 10)
			)
			((= recv_dialog_id 11)
			)
			((= recv_dialog_id 12)
			)
			((= recv_dialog_id 13);导入/恢复按键的操作
				(defun smm-output-dcl-value (self.data self.data_type / result)
					(cond
						((= self.data_type "LIST" )
						(if self.data
							(setq result (nth (1- (length self.data)) self.data))
						))
						((= self.data_type "STR" )
						(if self.data
							(cond
							((= (type self.data) 'STR) (setq result self.data))
							((= (type self.data) 'INT) (setq result (itoa self.data)))
							((= (type self.data) 'REAL) (setq result (rtos self.data)))
							)
						))
					)
				result   
				)
				(progn ;测试用代码
					(foreach saved_raw_data test_bakup_data
						(setq data_list (vls-string->list saved_raw_data ":"))
						(setq data_type (nth 1 data_list))
						(cond 
							((= data_type "INT") (set (read (nth 0 data_list)) (atoi (nth 2 data_list)) ))
							((= data_type "REAL") (set (read (nth 0 data_list)) (atof (nth 2 data_list)) ))
							((= data_type "STR") (set (read (nth 0 data_list)) (nth 2 data_list) ))
							((= data_type "LIST") (set (read (nth 0 data_list)) (read (nth 2 data_list)) ))
            ) 
	        )
					(PROGN
						(setq location_name_value (smm-output-dcl-value *location_name_list "LIST"))
						(setq soil_limits_value (smm-output-dcl-value *soil_limits_list "LIST"))
						(setq intense_limits_value (smm-output-dcl-value *intense_limits_list "LIST"))
						(setq moderate_limits_value (smm-output-dcl-value *moderate_limits_list "LIST"))
						(setq structure_point_type (smm-output-dcl-value *structure_point_type_list "LIST"))
						(setq StructurePointName_value (smm-output-dcl-value *structure_point_name_list "LIST"))
						(setq LastGeoCode_value (smm-output-dcl-value *last_geo_code_list "LIST"))
						(setq NextGeoCode_value (smm-output-dcl-value *next_geo_code_list "LIST"))
						(setq NextGeoCode_value (smm-output-dcl-value *next_geo_code_list "LIST"))
						(setq PlanScale_value (smm-output-dcl-value *plan_scale "STR"))
						(setq SectionScale_value (smm-output-dcl-value *section_scale "STR"))
						(if *plan_line_handle ;导入平面剖线
							(progn
								(setq *plan_line_in_plan (handent *plan_line_handle))
         				(setq ent_type (cdr (assoc 0 (entget *plan_line_in_plan))) )
								(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
									(progn
										(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
										(setq SelectSectionLine_value "已选择")
									)
								)
              )
						)
						(if *section_line_handle ;导入平面剖线
							(progn
								(setq *section_line_ground (handent *section_line_handle))
								(setq ent_type (cdr (assoc 0 (entget *section_line_ground))) )
								(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
								(progn
									(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
									(setq SelectSectionGround_value "已选择")
								)
								)
              )
						)
          )
					(if (= recover_status "OK")
						(alert "recovery ok")
					)
			 	)
			(smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 14)
			)
			((= recv_dialog_id 15);导出/备份按键的操作
       (if (and IMPORT_FILE_PATH (!= IMPORT_FILE_PATH "NULL"))
				(princ)
        (setq IMPORT_FILE_PATH (getfiled "选择恢复数据" "" "vlsd1" 48) )
       )
       (setq raw_data (open IMPORT_FILE_PATH "w"))
       (setq saved_data_list '())
       (foreach saved_data  *saved_variants
					(if saved_data
					(setq saved_data_list 
						(append saved_data_list
							(list (strcat (vl-princ-to-string saved_data) ":" (vl-princ-to-string (type (read saved_data))) ":" (vl-princ-to-string (eval (read saved_data))))) )
					))
       )
       (smm-load-menu-window dcl_id)
			)
		)
  )
	(defun smm-send-global-value () ;特殊情况下，把控件对应的值传递到非局部变量参数；
		;(setq draw_scale  (read drawingScaleInput_value))
	)
	(defun smm-load-menu-window (dcl_id / );加载菜单
		(setq dialog_return_id 2 dialog_name "StartMenu")
		(setq dcl_key_list '("AddFeaturePoint" "DeleteLastPoint" "RepickFeaturePoint" "SelectSectionLine" "isSelectSectionLine" "SelectSectionGround" "isSelectSectionGround" "PlanScale" "SectionScale" "ImportBakupData" "BottonForImport" "OutputBakupData" "BottonForOutput"))
    (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		; (while (> dialog_return_id 1) ; dialogStatus 为1时，是按了确定，0是按了取消
			(if (not (new_dialog dialog_name dcl_id)) (exit))
			(foreach key dcl_key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				);控件内容
				(action_tile key "(smm-action-tile->function $key)");点击动作
			)
			(setq dialog_return_id (start_dialog))
			(unload_dialog dcl_id)
			(smm-run-function-by-dialog-id dialog_return_id)
		; )
	)
  (smm-load-menu-window dcl_id)
)


;-----function---



;-----------------
; (defun save-stratum-limits-point-value( / i ) ;"location_name" "soil_limits" "intense_limits" "moderate_limits"
;   (alert "start")
;   (setq curr_soil_limits_list (vls-string->list soil_limits_value ","))
; 	(setq count_soil (length curr_soil_limits_list))
;   (setq i 0)
;   (while (< i count_soil)
; 		(set (read (strcat "soil_limits_list_" (itoa i))) (append (eval (read (strcat "soil_limits_list_" (itoa i)))) (list (nth i curr_soil_limits_list)) ))
;     (setq i (1+ i))
;   )
; )


(defun get-start-point-coefficient (obj_selected_line selected_point / result);取得选择起点与实际起点的距离（绝对值）
  (setq result (abs (- (vla-get-Length obj_selected_line) (vlax-curve-getdistatpoint obj_selected_line selected_point) )))
  result
)

(defun get-limits-index-lists ( / temp_dist temp_dist);对厚度列表组进行排序，得到排序序号的列表{limits_dist_index_list}和第二位开始的列表{next_limits_index_list}
	(foreach temp_point *limits_point_list
		(setq temp_dist (vlax-curve-getdistatpoint *obj_plan_line_in_plan temp_point))
		(setq temp_dist (abs (- temp_dist delta_start_point_dist)) )
		(setq limits_dist_list (append limits_dist_list (list temp_dist)))
	) ;给土层厚度根据点取得距离；
	(setq *limits_dist_index_list (vl-sort-i limits_dist_list sign_comparator));各种列表排序
  (setq *next_limits_index_list (cdr *limits_dist_index_list))
)
(defun make-polyline ( point_list / vertices result);点表绘制多线程序
	(setq dxf_90  (cons 90 (length point_list)))
  (setq vertices (mapcar '(lambda (p) (cons 10 p)) point_list))
  ;; 创建多段线
  (setq result (entmakex
    (append
      '((0 . "LWPOLYLINE")  ; 实体类型
        (100 . "AcDbEntity")
        (100 . "AcDbPolyline")
        dxf_90            ; 顶点数量
        (70 . 0))           ; 多段线标志，0 表示开放
      vertices
    )
  ))
  result
)

(defun make-sectionwith-line (recv_x recv_y / top_point bottom_point temp_line)
	(setq top_point (append (list recv_x) (list (+ recv_y 5000)) ) bottom_point (append (list recv_x) (list (- recv_y 5000))) )
	(setq temp_line (entmakex
		(list
			(cons 0 "LINE")
			(cons 10 top_point)
			(cons 11 bottom_point)
		)
	))
  temp_line
)
(defun get-point-at-line (obj_pline recv_x recv_line / obj_line obj_intersection_points intersection_points );给一个值，求得线上的点值

	(setq obj_line (vlax-ename->vla-object recv_line))
	(setq obj_intersection_points (vlax-invoke
		obj_pline
		'IntersectWith
		obj_line
		acExtendNone
	))
	(if obj_intersection_points
		(setq intersection_points (vlax-safearray->list obj_intersection_points))
  	(progn
			(alert "fuction :{get-point-at-line}未找到交点。")
			(exit)
	))
	intersection_points
)
(defun get-draw-limits-lists ( / );给土厚度各种参数赋值
  ;局部变量 length_list x_increase_dist i j  first_soils second_soils x_increment insert_number soil_str first_soil_limits first_intense second_intense first_moderate second_moderate temp_move_point total_x_increment section_start_x_coordinate
	(defun input-draw-line-value (recv_coor_x recv_coor_y recv_soil_limits recv_intense recv_moderate / index_number )
		(setq index_number  0)
		(while (< index_number (length recv_soil_limits)) ; 土层赋值
			;{draw_soil_line_poiont_list};{draw_soil_line_poiont}
			(setq recv_coor_y (- recv_coor_y (nth index_number recv_soil_limits) ))
			(set (read (strcat "*draw_soil_line_" (itoa index_number))) (append (read (strcat "*draw_soil_line_" (itoa index_number))) (list recv_coor_x recv_coor_y 0)));各层土层点列表
		)
		(setq recv_coor_y (- recv_coor_y recv_intense));强风化下限
		(setq *draw_intense_line (append *draw_intense_line (list recv_coor_x recv_coor_y 0)));强风化线点表
		(setq recv_coor_y (- recv_coor_y recv_moderate));弱风化下限
		(setq *draw_moderate_line (append *draw_moderate_line (list recv_coor_x recv_coor_y 0)));弱风化线点表
	)
  (defun add-draw-limits-value (index_1st index_2nd / second_soil_limits first_soil_limits)
		(setq first_index_number (nth index_1st *limits_dist_index_list))
		(setq second_index_number (nth index_2nd *next_limits_index_list))
		(setq first_soils  (nth first_index_number *soil_limits_list));前一个点的层厚值
  	(setq first_soil_list_str (vls-string->list first_soils ","));土层列表
    (setq first_soil_limits '())
		(foreach soil_str first_soil_list_str
			(setq first_soil_limits (append first_soil_limits (list (atof soil_str))))
    );实数形式的土层厚度列表 {first_soil_limits}
		(setq second_soils  (nth second_index_number *soil_limits_list));后一个点的土层厚值
    (setq second_soil_list_str (vls-string->list second_soils ","));土层列表
    (setq second_soil_limits '())
		(foreach soil_str second_soil_list_str
			(setq second_soil_limits (append second_soil_limits (list (atof soil_str))))
    );实数形式的土层厚度列表 {second_soil_limits}
		(setq first_intense  (atof (nth first_index_number *intense_limits_list)));强风化厚度值
		(setq second_intense  (atof (nth second_index_number *intense_limits_list)));弱风化厚度值
		(setq first_moderate  (atof (nth first_index_number *moderate_limits_list)));强风化厚度值
		(setq second_moderate  (atof (nth second_index_number *moderate_limits_list)));弱风化厚度值

		(setq x_increase_dist  (* (atof (nth first_index_number limits_dist_list)) x_scale) );前一个点的距离值，x的加值
		(cond
			((> x_increase_dist 2000) (setq insert_number (1- (/ (fix x_increase_dist) 100))) )
			((> x_increase_dist 1000) (setq insert_number (1- (/ (fix x_increase_dist) 30 ))) )
			((> x_increase_dist 100 ) (setq insert_number (1- (/ (fix x_increase_dist) 15 ))) )
			((> x_increase_dist 10  ) (setq insert_number (1- (/ (fix x_increase_dist) 5  ))) )
			(T (setq insert_number 2) )
    );内插个数，按x距离进行判断,{insert_number}
		(setq x_increment (/ x_increase_dist insert_number));内插距离,即x的增加距离
		(setq curr_soils_length (length second_soil_limits))
	
    (setq j  0)
  	(while (< j curr_soils_length)
			(set (read (strcat "first_soil_" (itoa j))) (nth j first_soil_limits) )
			(set (read (strcat "second_soil_" (itoa j))) (nth j second_soil_limits) )
      (set (read (strcat "soil_increment_" (itoa j) )) (/ (* (- (nth j second_soil_limits) (nth j first_soil_limits)) y_scale) (1+ y_scale)) )
			(setq j  (1+ j))
    );求得土层第层的内插y值 {soil_increment_[序号]}
  	(setq intense_increment (/ (* (- second_intense first_intense) y_scale) (1+ insert_number)) );强风化插值{intense_increment}
  	(setq moderate_increment (/ (* (- second_moderate first_moderate) y_scale) (1+ insert_number)) );弱风化插值{moderate_increment}  
		(setq temp_coor_x section_start_x_coordinate);复制现在的X距离
		(setq section_start_x_coordinate (+ section_start_x_coordinate x_increase_dist));将第一个点移到最后一个点，即下一循环的开始点
		
		(setq curr_coor_x temp_coor_x);地面点X
		(setq curr_soil_limits first_soil_limits)
		(setq curr_intense first_intense)
   	(setq curr_moderate second_moderate)
		(while (< curr_coor_x section_start_x_coordinate) ;各值进列表
			(setq curr_coor_y (cadr (get-point-at-line *obj_section_line_ground curr_coor_x intersectionwith_line)) );求得插值点的点坐标
			(input-draw-line-value curr_coor_x curr_coor_y curr_soil_limits curr_intense curr_moderate)
    ;---fold---
			; 	(setq j  0)
			; 	(while (< j (length curr_soil_limits)) ; 土层赋值
			; 		;{draw_soil_line_poiont_list};{draw_soil_line_poiont}
			; 		(setq curr_coor_y (- curr_coor_y (nth j curr_soil_limits) ))
			; 		(set (read (strcat "*draw_soil_line_" (itoa j))) (append (read (strcat "*draw_soil_line_" (itoa j))) (list curr_coor_x curr_coor_y 0)));各层土层点列表
			;   )
			; 	(setq curr_coor_y (- curr_coor_y curr_intense));强风化下限
			;   (setq *draw_intense_line (append *draw_intense_line (list curr_coor_x curr_coor_y 0)))
			; 	(setq curr_coor_y (- curr_coor_y curr_moderate));弱风化下限
			;   (setq *draw_moderate_line (append *draw_moderate_line (list curr_coor_x curr_coor_y 0)))
    ;
			(vl-cmdf "move" intersectionwith_line "" temp_move_point  x_increment)
			(setq curr_coor_x (+ x_increment curr_coor_x));地面点X
		)
		;准备下一循环（下一个两点的循环）

   	(entdel intersectionwith_line)
		(setq total_x_increment (+ total_x_increment x_increase_dist))
		(vl-cmdf "copy" at_start_point_line "" 0 total_x_increment)
		(setq intersectionwith_line (entlast))
		(setq i (1+ i))
	)
  (defun start-get-draw-limits-lists ( / )
		(setq length_list (length *next_limits_index_list) i 0)
		(setq temp_coor_y (cadr *section_start_point));这里赋值剖面起点的Y坐标
		(setq section_start_x_coordinate (car *section_start_point))
		(setq total_x_increment 0);总距离 ，用来复制{at_start_point_line}到{intersectionwith_line}
		(setq temp_move_point (list section_start_x_coordinate temp_coor_y));移动基点
		(setq at_start_point_line (make-sectionwith-line section_start_x_coordinate temp_coor_y));构造一条交线以求y值
		(vl-cmdf "copy" at_start_point_line "" 0 0)
		(setq intersectionwith_line (entlast));复制线段到需要接交点的距离处
		;--进入赋值循环
		(while (< i length_list)
			;-----fold---
				; (setq first_index_number (nth i *limits_dist_index_list))
				; (setq second_index_number (nth i *next_limits_index_list))
				; (setq first_soils  (nth first_index_number *soil_limits_list));前一个点的层厚值
				; (setq first_soil_list (vls-string->list first_soils ","));土层列表
				; (foreach soil_str first_soil_list
				; 	(setq first_soil_limits (append first_soil_limits (list (atof soil_str))))
				; );实数形式的土层厚度列表 {first_soil_limits}
				; (setq second_soils  (nth second_index_number *soil_limits_list));后一个点的土层厚值
				; (setq second_soil_list (vls-string->list second_soils));土层列表
				; (foreach soil_str second_soil_list
				; 	(setq second_soil_limits (append second_soil_limits (list (atof soil_str))))
				; );实数形式的土层厚度列表 {second_soil_limits}
				; (setq first_intense  (atof (nth first_index_number *intense_limits_list)));强风化厚度值
				; (setq second_intense  (atof (nth second_index_number *intense_limits_list)));弱风化厚度值
				; (setq first_moderate  (atof (nth first_index_number *moderate_limits_list)));强风化厚度值
				; (setq second_moderate  (atof (nth second_index_number *moderate_limits_list)));弱风化厚度值

				; (setq x_increase_dist  (* (atof (nth first_index_number limits_dist_list)) x_scale) );前一个点的距离值，x的加值
				; (cond
				; 	((> x_increase_dist 2000) (setq insert_number (1- (/ (fix x_increase_dist) 100))) )
				; 	((> x_increase_dist 1000) (setq insert_number (1- (/ (fix x_increase_dist) 30 ))) )
				; 	((> x_increase_dist 100 ) (setq insert_number (1- (/ (fix x_increase_dist) 15 ))) )
				; 	((> x_increase_dist 10  ) (setq insert_number (1- (/ (fix x_increase_dist) 5  ))) )
				; 	(T (setq insert_number 2) )
				; );内插个数，按x距离进行判断,{insert_number}
				; (setq x_increment (/ x_increase_dist insert_number));内插距离,即x的增加距离
				; (setq curr_soils_length (length second_soil_limits))
				
			
				; (setq j  0)
				; (while (< j curr_soils_length)
				; 	(set (read (strcat "first_soil_" (itoa j))) (nth j first_soil_limits) )
				; 	(set (read (strcat "second_soil_" (itoa j))) (nth j second_soil_limits) )
				;   (set (read (strcat "soil_increment_" (itoa j) )) (/ (* (- (nth j second_soil_limits) (nth j first_soil_limits)) y_scale) (1+ y_scale)) )
				; 	(setq j  (1+ j))
				; );求得土层第层的内插y值 {soil_increment_[序号]}
				; (setq intense_increment (/ (* (- second_intense first_intense) y_scale) (1+ insert_number)) );强风化插值{intense_increment}
				; (setq moderate_increment (/ (* (- second_moderate first_moderate) y_scale) (1+ insert_number)) );弱风化插值{moderate_increment}  
				; (setq temp_coor_x section_start_x_coordinate);复制现在的X距离
				; (setq section_start_x_coordinate (+ section_start_x_coordinate x_increase_dist));将第一个点移到最后一个点，即下一循环的开始点
				
				; (setq curr_coor_x temp_coor_x);地面点X
				; (setq curr_soil_limits first_soil_limits)
				; (setq curr_intense first_intense)
				; (setq curr_moderate second_moderate)
				; (while (< curr_coor_x section_start_x_coordinate) ;各值进列表
				; 	(setq curr_coor_y (cadr (get-point-at-line *obj_section_line_ground curr_coor_x intersectionwith_line)) );求得插值点的点坐标
				; 	(setq j  0)
				; 	(while (< j (length curr_soil_limits)) ; 土层赋值
				; 		;{draw_soil_line_poiont_list};{draw_soil_line_poiont}
				; 		(setq curr_coor_y (- curr_coor_y (nth j curr_soil_limits) ))
				; 		(set (read (strcat "*draw_soil_line_" (itoa j))) (append (read (strcat "*draw_soil_line_" (itoa j))) (list curr_coor_x curr_coor_y 0)));各层土层点列表
				;   )
				; 	(setq curr_coor_y (- curr_coor_y curr_intense));强风化下限
				;   (setq *draw_intense_line (append *draw_intense_line (list curr_coor_x curr_coor_y 0)))
				; 	(setq curr_coor_y (- curr_coor_y curr_moderate));弱风化下限
				;   (setq *draw_moderate_line (append *draw_moderate_line (list curr_coor_x curr_coor_y 0)))

				; 	(vl-cmdf "move" intersectionwith_line "" temp_move_point  x_increment)
				; 	(setq curr_coor_x (+ x_increment curr_coor_x));地面点X
				; )
				; ;准备下一循环（下一个两点的循环）
				; (entdel intersectionwith_line)
				; (setq total_x_increment (+ total_x_increment x_increase_dist))
				; (vl-cmdf "copy" at_start_point_line "" 0 total_x_increment)
				; (setq intersectionwith_line (entlast))
			;
			(add-draw-limits-value i i)
			(setq i (1+ i))
		)
		;添加最后一个点的值
		(progn ;添加最后一个点的值
			(setq curr_coor_x (car *section_end_point  ));地面点X
			(setq curr_coor_y (cadr *section_end_point));地面点X
			(setq first_soils  (nth i *soil_limits_list));前一个点的层厚值
			(setq curr_soil_limits_str (vls-string->list first_soils ","));土层列表
			(setq curr_soil_limits '())
			(foreach soil_str curr_soil_limits_str
					(setq curr_soil_limits (append curr_soil_limits (list (atof soil_str))))
			)
			(setq *soil_count (length curr_soil_limits))
			(setq curr_intense (atof (nth i *intense_limits_list)))
			(setq curr_moderate (atof (nth i *intense_limits_list)))
			(input-draw-line-value curr_coor_x curr_coor_y curr_soil_limits curr_intense curr_moderate)
		)
	)
  (start-get-draw-limits-lists)
)
(defun draw-soil-line ()
	(get-draw-limits-lists)
	(setq i 0)
  (while (< i *soil_count)
    (make-polyline (read (strcat "*draw_soil_line_" (itoa i))))
		(1+ i)
  )
	(make-polyline  *draw_intense_line)
	(make-polyline  *draw_intense_line)
)



(defun init-variant ()
  (setq DCL_PATH "W:\\Plugin\\LISP\\AutoGeoDraw\\AutoGeologicalProfilePlot\\AGPPMenu.dcl")
  (setq dcl_id 999)
  (setq *saved_variants '("*plan_start_point" "*plan_end_point" "*plan_line_handle" "*section_start_point" "*section_end_point" "*section_line_handle" "*location_name_list" "*soil_limits_list" "*intense_limits_list" "*moderate_limits_list" "*limits_point_list" "*structure_point_type_list" "*structure_point_name_list" "*last_geo_code_list" "*next_geo_code_list" "*structure_point_list" "*plan_scale" "*section_scale" "*data_saved_path" "*data_import_path" "IMPORT_FILE_PATH"))
  (setq IMPORT_FILE_PATH '())
  (setq OUTPUT_FILE_PATH '())
  ;----fold---
		; (setq feature_point_list '()) ;-> 特征点列表;
		; (setq elevation_interpolate_list '()) ;-> 高程插值列表;
		; (setq geological_boundary_point_list '()) ;-> 地层分界线特征点列表
		; (setq *soil_limits_list '()) ;-> 土层下限列表;
		; (setq location_name_list '()) ;-> 土层点名称列表;
		; (setq intense_limits_list '()) ;-> 强风化下限列表;
		; (setq moderate_limits_list '()) ;-> 弱风化下限列表;
		; (setq limits_point_list '()) ;-> 厚度点列表;
		; (setq limits_point_x_list '()) ;-> 厚度点列表;
		; (setq limits_point_y_list '()) ;-> 厚度点列表;
		
		; (setq last_select_point_type_list '()) ;-> 最后地层分界线特征点信息；
		; (setq last_select_point_type 1);最后/当前特征点类型，1为土层厚度点，2为地质构造点
		(setq structure_point_type 3) ;3，地质分界线 4，断层 5，褶皱
		; (setq structure_point_type_list '());地质结构类型列表
		; (setq structure_point_name_list '());地质点名称列表
		; (setq last_geo_code_list '());结构线前地质名称型列表
		; (setq next_geo_code_list '());结构线后地质名称型列表
		; (setq structure_point_list '());地质结构特征点列表
		; (setq structure_point_x_list '());地质结构特征点列表
		; (setq structure_point_y_list '());地质结构特征点列表
		; (setq soil_limits_value "3," intense_limits_value "3" moderate_limits_value "6") ;初始化土层厚度
		; (setq section_line_in_plan nil);平面图上的剖面线
		; (setq obj_section_line_in_plan nil);平面图上的剖面线对象
		; (setq *section_line_ground nil);剖面图上的剖面线
		; (setq obj_*section_line_ground nil);剖面图上的剖面线对象
		; (setq *plan_start_point '());平面图上的剖面起点
		; (setq *plan_end_point '());平面图上的剖面终点
		; (setq *section_start_point '());剖面图上的剖面起点
		; (setq *section_end_point '());剖面图上的剖面终点
  ;
)


(defun set-variant ( / )
  (setq *plan_scale (atof PlanScale_value));平面图的比例
  (setq *section_scale (atof SectionScale_value));剖面图的比例
  (setq x_scale (/ *plan_scale *section_scale));横向比例
  (setq y_scale (/ 1000 *section_scale));纵向比例
	(setq delta_start_point_dist (get-start-point-coefficient *obj_plan_line_in_plan *plan_start_point));起点的差值，主要是剖面线起点与线段起点可能不重合
  (setq section_start_x_coordinate (car *section_start_point))
  (if (> delta_start_point_dist 0) ;根据起点方向确定排序
		(setq sign_comparator (quote >))
		(setq sign_comparator (quote <))
	)
	(get-limits-index-lists);{limits_dist_list},{limits_dist_index_list}
  (get-draw-limits-lists);
)

; (defun validation-variable-exist ( / ) ;验证变更完整性；
; 	(princ "验证并补全基础数据")
; )

(defun print-variant (variant_list / i )
  (setq list_length (length variant_list))
  (foreach var variant_list)
	(princ (strcat (read var) "is:" var "\n"))
  (princ)
)



; (defun foo ( / )
; 	(setq point_distance (obj_section_line_in_plan obj_*section_line_ground *get_point) );计算离起点的初始距离，未进行差值计算
; 	(setq point_distance_list (append point_distance_list (list point_distance) ));初始距离-->初始距离列表
; )

;-------------------------
(defun start-program ( / )
  (setq test_bakup_data '("*plan_line_handle:STR:2CEE" "*section_line_handle:STR:2CF0" "*location_name_list:LIST:(pt1 pt2 pt2 11 12)" "*soil_limits_list:LIST:(3,4,5 3,4,6 3,4,3 3,4,3 3,4,3)" "*intense_limits_list:LIST:(2 3 4 4 4)" "*moderate_limits_list:LIST:(4 7 5 5 5)" "*limits_point_list:LIST:((307.961 1647.39 0.0) (509.677 1849.11 0.0) (942.09 2281.52 0.0) (1194.5 2533.93 0.0) (1418.96 2758.39 0.0))" "*structure_point_type_list:LIST:(3 4 5)" "*structure_point_name_list:LIST:(pt1 pt2 4)" "*last_geo_code_list:LIST:(1 3 5)" "*next_geo_code_list:LIST:(2 4 6)" "*structure_point_list:LIST:((523.755 1863.19 0.0) (736.945 2076.38 0.0) (1181.43 2520.86 0.0))" "*plan_scale:STR:50000.0" "*section_scale:STR:500.0" "*data_saved_path:STR:nil" "*data_import_path:STR:nil" "recover_status:STR:OK"))
	(init-variant)
	(start-main-menu dcl_id)
  ; (validation-variable-exist)
	(set-variant)
  ; (print-variant variant_list)
  (alert "start_drawing")
  (draw-soil-line)
  ; (draw-intense-line)
  ; (draw-moderate-line)·
  ; (draw-structure-line)
  ; (output-data)
  ; (test)
  
  
  (print-variant)
)


(defun c:test( / )
  (alert "vmodify: 2024-09-13 00:00")
      ; (setq saved_data_list '())
      ;  (foreach saved_data  *saved_variants
			; 		(setq saved_data_list 
			; 			(append saved_data_list
      ;       	(list (strcat (vl-princ-to-string saved_data) ":" (vl-princ-to-string (type (read saved_data))) ":" (vl-princ-to-string (eval (read saved_data))))) )
			; 		)
      (defun smm-output-dcl-value (self.data self.data_type / result)
        (cond
					((= self.data_type "LIST" )
					(if self.data
						(setq result (nth (1- (length self.data)) self.data))
					))
					((= self.data_type "STR" )
					(if self.data
						(cond
           	((= (type self.data) 'STR) (setq result self.data))
           	((= (type self.data) 'INT) (setq result (itoa self.data)))
           	((= (type self.data) 'REAL) (setq result (rtos self.data)))
						)
					))
				)
       result   
			)
				(progn ;测试用代码
      		(setq test_bakup_data '("*plan_line_handle:STR:2CEE" "*section_line_handle:STR:2CF0" "*location_name_list:LIST:(pt1 pt2 pt2 11 12)" "*soil_limits_list:LIST:(3,4,5 3,4,6 3,4,3 3,4,3 3,4,3)" "*intense_limits_list:LIST:(2 3 4 4 4)" "*moderate_limits_list:LIST:(4 7 5 5 5)" "*limits_point_list:LIST:((307.961 1647.39 0.0) (509.677 1849.11 0.0) (942.09 2281.52 0.0) (1194.5 2533.93 0.0) (1418.96 2758.39 0.0))" "*structure_point_type_list:LIST:(nil 4 5)" "*structure_point_name_list:LIST:(pt1 pt2 4)" "*last_geo_code_list:LIST:(1 3 5)" "*next_geo_code_list:LIST:(2 4 6)" "*structure_point_list:LIST:((523.755 1863.19 0.0) (736.945 2076.38 0.0) (1181.43 2520.86 0.0))" "*plan_scale:STR:50000.0" "*section_scale:STR:500.0" "*data_saved_path:STR:nil" "*data_import_path:STR:nil"))
					(foreach saved_raw_data test_bakup_data
						(setq data_list (vls-string->list saved_raw_data ":"))
						(setq data_type (nth 1 data_list))
						(cond 
							((= data_type "INT") (set (read (nth 0 data_list)) (atoi (nth 2 data_list)) ))
							((= data_type "REAL") (set (read (nth 0 data_list)) (atof (nth 2 data_list)) ))
							((= data_type "STR") (set (read (nth 0 data_list)) (nth 2 data_list) ))
							((= data_type "LIST") (set (read (nth 0 data_list)) (read (nth 2 data_list)) ))
            )
                   
          
          
          )
					(PROGN
						(setq location_name_value (smm-output-dcl-value *location_name_list "LIST"))
						(setq soil_limits_value (smm-output-dcl-value *soil_limits_list "LIST"))
						(setq intense_limits_value (smm-output-dcl-value *intense_limits_list "LIST"))
						(setq moderate_limits_value (smm-output-dcl-value *moderate_limits_list "LIST"))
						(setq structure_point_type (smm-output-dcl-value *structure_point_type_list "LIST"))
						(setq StructurePointName_value (smm-output-dcl-value *structure_point_name_list "LIST"))
						(setq LastGeoCode_value (smm-output-dcl-value *last_geo_code_list "LIST"))
						(setq NextGeoCode_value (smm-output-dcl-value *next_geo_code_list "LIST"))
						(setq NextGeoCode_value (smm-output-dcl-value *next_geo_code_list "LIST"))
						(setq PlanScale_value (smm-output-dcl-value *plan_scale "STR"))
						(setq SectionScale_value (smm-output-dcl-value *section_scale "STR"))
						(if *plan_line_handle ;导入平面剖线
							(progn
								(setq *plan_line_in_plan (handent *plan_line_handle))
         				(setq ent_type (cdr (assoc 0 (entget *plan_line_in_plan))) )
								(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
									(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
									(setq SelectSectionLine_value "已选择")
								)
              )
						)
						(if *section_line_handle ;导入平面剖线
							(progn
								(setq *section_line_ground (handent *section_line_handle))
								(setq ent_type (cdr (assoc 0 (entget *section_line_ground))) )
								(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
									(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
									(setq SelectSectionGround_value "已选择")
								)
              )
						)
          )
			 	)
					; 	(cond
					; 		((= data_type 'INT)  (progn (setq saved_type "INT") (setq saved_data (itoa saved_data))))
					; 		((= data_type 'REAL) (progn (setq saved_type "REAL") (setq saved_data (rtos saved_data)))
					; 		((= data_type 'STR)  (setq saved_type "STR"))
					; 		((= data_type 'LIST) (setq saved_type "LIST"))
					; 		(T                   (setq saved_type "OTHER"))
					; )
					; (setq saved_data_list
					; 	(append saved_data_list 
					; 		(list
					; 			(strcat saved_data ":" saved_type ":" (read "saved_data"))))
					; )
    		; )
  
  
  
	; (init-variant)
	; (start-main-menu dcl_id)
  
  ; (setq file_path "Y:\\Temp\\testdata.vlsd1")
  ; (princ saved_data_list)
  ; (setq file_obj (open file_path "r"))
	; (if file_obj
	; (progn
	; 	(while (setq line (read-line file_obj))
	; 		(princ line))
	; 	(close file_obj))
	; (princ "\n无法打开文件。"))
  ; (validation-variable-exist)
	; (set-variant)
  ; (print-variant variant_list)
)










;命令程序
(defun C:TT (/ *error* *CurDoc* *Old_SysVar*) ;->程序启动程序
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
						(vls-Modify-SystemVariant) ; vls-Core内函数
			);记录参数
		)
		)
  (setq ACADOBJ (vlax-get-acad-object)
          DOC (vla-get-ActiveDocument ACADOBJ))
	(start-program)
	(command "redraw");清空一行
	(if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
	(vla-endundomark *CurDoc*) ;结束编组
	(princ)
)