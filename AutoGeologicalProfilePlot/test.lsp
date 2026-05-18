; geological_profile_plot
; 地质剖面绘图插件 - 用于在AutoCAD中绘制地质剖面图

;--------------------------------
; 加载必要的ActiveX扩展和自定义库
(if (null vlax-dump-object) (vl-load-com))
(if (null vls-core-load) (load "TbxlsCore"))
(if (null vls-text-load) (load "TbxlsText"))

;-----dcl-----
; 选择特征点类型对话框 - 判断选择的是界限点还是结构点
(defun select-feature-point-type (dcl_id / sfpt-set-all-key-value sfpt-action-tile->function sfpt-run-function-by-dialog-id sfpt-load-menu-window key_list)
    ; 设置所有已编辑控件的值到对应的全局变量
	(defun sfpt-set-all-key-value ( / dcl_key )
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
    ; 全局按钮的动作处理 - 根据不同key值触发对应的button-action响应
	(defun sfpt-action-tile->function (recv_key)
		(cond
			((= recv_key "cancel") ; 取消按钮
				(done_dialog 0)
			)
			((= recv_key "accept") ; 确认按钮
				(done_dialog 1)
			)
			((= recv_key "help") ; 帮助按钮
				(sfpt-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "LimitsPoint") ; 界限点按钮
				(sfpt-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "StructurePoint") ; 结构点按钮
				(sfpt-set-all-key-value)
				(done_dialog 4)
			)
		)
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun sfpt-run-function-by-dialog-id (dcl_id / ss_length)
		(cond
			((= dcl_id 0) ; 返回主菜单
                (start-main-menu dcl_id)
			)
			((= dcl_id 1) ; 返回主菜单
                (start-main-menu dcl_id)
			)
			((= dcl_id 3) ; 选择界限点类型
                (setq current_select_point_type 1) ; 设置为界限点类型
                (setq select_point_type_list (append select_point_type_list (list current_select_point_type))) ; 添加到选择列表
                (input-stratum-limits-point dcl_id)
			)
			((= dcl_id 4) ; 选择结构点类型
                (setq current_select_point_type 2) ; 设置为结构点类型
				(setq select_point_type_list (append select_point_type_list (list current_select_point_type))) ; 添加到选择列表
                (input-structure-point dcl_id)
			)
		)
	)
    ; 导出全局变量值到局部函数 - 用于数据传递
	(defun sfpt-send-global-value ()
		;(setq draw_scale  (read drawingScaleInput_value))
	)
    ; 加载对话框菜单
	(defun sfpt-load-menu-window (dcl_id / )
		(setq dialog_name "SelectPointType")
		(setq dcl_key_list '("LimitsPoint"  "StructurePoint"))
        (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "NULL")
			); 控件回显
			(action_tile key "(sfpt-action-tile->function $key)"); 动作注册
		)
		(setq dialog_return_id (start_dialog))
		(sfpt-run-function-by-dialog-id dialog_return_id)
		(unload_dialog dcl_id)
	)
  (sfpt-load-menu-window dcl_id)
)

; 输入地层界限点数据 - 收集位置名称、土壤界限、强度界限、缓动界限等数据
(defun input-stratum-limits-point ( dcl_id / isl-key-value-get isl-action-format-keys isl-botton-action isl-get-input-value isl-load-menu key_list dialog_return_id)
    ; 获取所有控件的编辑值
	(defun isl-key-value-get ( / temp_key )
		(foreach key key_list
			(set (read (strcat key "_value")) (get_tile key))
		)
	)
    ; 按钮动作处理 - 根据不同key值处理对应的业务逻辑
	(defun isl-action-format-keys (key)
		(cond
			((= key "cancel") ; 取消按钮
				(done_dialog 0); 对话框退出，返回数据给Dialog_Return值为0
			)
			((= key "accept") ; 确认按钮
                (isl-key-value-get)
				; (save-stratum-limits-point-value)
			 	(done_dialog 1)
			)
        	((= key "location_name") ; 位置名称输入
                (isl-key-value-get)
			 	; (done_dialog 3)
			)
        	((= key "soil_limits") ; 土壤界限输入
                (isl-key-value-get)
			 	; (done_dialog 4)
			)
			((= key "intense_limits") ; 强度界限输入
                (isl-key-value-get)
			 	; (done_dialog 5)
			)
	    	((= key "moderate_limits") ; 缓动界限输入
                (isl-key-value-get)
			 	; (done_dialog 6)
			)
	    )
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun isl-botton-action (isl_botton_id /)
        (cond
			( (= isl_botton_id 0); 返回主菜单
              	(start-main-menu dcl_id)
            )
			( (= isl_botton_id 1); 保存数据并返回主菜单
				; 保存值
				(setq *location_name_list (append *location_name_list (list location_name_value)) ); 位置名称列表-->位置名称列表
				(setq *soil_limits_list (append *soil_limits_list (list soil_limits_value)) ); 土壤界限列表-->土壤极限字符串列表
				(setq *intense_limits_list (append *intense_limits_list (list intense_limits_value)) ); 强度界限列表-->强度数值列表
				(setq *moderate_limits_list (append *moderate_limits_list (list moderate_limits_value)) ); 缓动界限列表-->缓动数值列表
				(setq *limits_point_list (append *limits_point_list (list *get_point))); 平面图上的点位-->点位列表=
				; 清空值
				; (setq limits_point_x_list (append limits_point_x_list (list (car *get_point))))
				; (setq limits_point_y_list (append limits_point_y_list (list (cadr *get_point))))
				(start-main-menu dcl_id)
            )
		)
  )
  ; (defun isl-get-input-value () ; 获取edit_box的输入值
	; 	(princ "str")
	; )
    ; 加载对话框菜单
	(defun isl-load-menu (dcl_id /  )
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
				); 控件回显
				(if (or (= (length *soil_limits_list) "NULL") (= (length *soil_limits_list) 0))
						(setq location_name_value (strcat "pt" (itoa 1)))
						(setq location_name_value (strcat "pt" (itoa (1+ (length *soil_limits_list)))))
                )
				(action_tile key "(isl-action-format-keys $key)"); 动作注册
			)
			(setq action_status (start_dialog))
		)
		(unload_dialog dcl_id)
		(isl-botton-action action_status)
  )
  (isl-load-menu dcl_id)
)

; 输入结构点数据 - 处理边界点、断裂点、褶皱点等信息
(defun input-structure-point (dcl_id / isp-set-all-key-value isp-action-tile->function isp-run-function-by-dialog-id isp-load-menu-window dcl_key_list)
    ; 设置所有已编辑控件的值到对应的全局变量
	(defun isp-set-all-key-value ( / dcl_key )
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
    ; 全局按钮的动作处理
	(defun isp-action-tile->function (recv_key)
		(cond
			((= recv_key "cancel") ; 取消按钮
				(done_dialog 0)
			)
			((= recv_key "accept") ; 确认按钮
				(done_dialog 1)
			)
			((= recv_key "help") ; 帮助按钮
				(isp-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "IsBoundary") ; 边界点
				(isp-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "IsFault") ; 断裂点
				(isp-set-all-key-value)
				(done_dialog 4)
			)
			((= recv_key "IsFold") ; 褶皱点
				(isp-set-all-key-value)
				(done_dialog 5)
			)
        	((= recv_key "StructurePointName") ; 结构点名称
				(isp-set-all-key-value)
				; (done_dialog 6)
			)
			((= recv_key "LastGeoCode") ; 上一个地质代码
				(isp-set-all-key-value)
				; (done_dialog 7)
			)
			((= recv_key "NextGeoCode") ; 下一个地质代码
				(isp-set-all-key-value)
				; (done_dialog 8)
			)
		)
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun isp-run-function-by-dialog-id (recv_dialog_id / ss_length)
		(cond
			((= recv_dialog_id 0)
              	(start-main-menu dcl_id)
			)
			((= recv_dialog_id 1)
               "IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"
              	(setq *structure_point_type_list (append *structure_point_type_list (list structure_point_type) ))
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
              	(alert "边界点")
               	(setq structure_point_type 3)
			)
        	((= recv_dialog_id 4)
              	(alert "断裂")
               	(setq structure_point_type 4)
			)
			((= recv_dialog_id 5)
              	(alert "褶皱")
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
    ; 导出全局变量值
	(defun isp-send-global-value ()
		;(setq draw_scale  (read drawingScaleInput_value))
	)
    ; 加载结构点输入对话框
	(defun isp-load-menu-window (dcl_id / )
		(setq dialog_status 2 dialog_name "InputStructure")
		(setq dcl_key_list '("IsBoundary" "IsFault" "IsFold" "StructurePointName" "LastGeoCode" "NextGeoCode"))
        (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(while (> dialog_status 1) ; dialogStatus 为1时表示当前按确认，0是按取消
			(if (not (new_dialog dialog_name dcl_id)) (exit))
			(foreach key dcl_key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				); 控件回显
				(if (or (= StructurePointName_value "NULL") (= (length *structure_point_name_list) 0))
					(setq StructurePointName_value "pt1")
					(setq StructurePointName_value (strcat "pt" (itoa (1+ (length *structure_point_name_list)))))
				)
				(action_tile key "(isp-action-tile->function $key)"); 动作注册
			)
			(setq dialog_status (start_dialog))
			(isp-run-function-by-dialog-id dialog_status)
		)
		(unload_dialog dcl_id)
	)
  (isp-load-menu-window dcl_id)
)

; 确认结束对话框 - 确认是否完成选择
(defun menu-confrim-end (dcl_id / set-all-key-value mce-action-tile->function mce-run-function-by-dialog-id mce-load-menu-window dcl_key_list dcl_id)
    ; 按钮动作处理
	(defun mce-action-tile->function (recv_key)
		(cond
			((= recv_key "cancel") ; 取消按钮
				(done_dialog 0)
			)
			((= recv_key "accept") ; 确认按钮
                (done_dialog 1)
			)
		)
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun mce-run-function-by-dialog-id (recv_dialog_id / ss_length)
		(cond
			((= recv_dialog_id 0)
               (setq ConfirmEndText_value "是否完成选择？")
               (start-main-menu dcl_id)
			)
			((= recv_dialog_id 1)
				(setq ConfirmEndText_value "是否完成选择？")
			)
		)
	)
    ; 加载确认结束对话框
	(defun mce-load-menu-window (dcl_id / )
		(setq dcl_id 2 dialog_name "ConfirmEnd")
        (setq dcl_key_list '("ConfirmEndText"))
		(setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "是否完成选择？")
			); 控件回显
			(action_tile key "(mce-action-tile->function $key)"); 动作注册
		)
		(setq dcl_id (start_dialog))
		(mce-run-function-by-dialog-id dcl_id)
	)
  (unload_dialog dcl_id)
  (mce-load-menu-window dcl_id)
)

; 确认取消对话框 - 确认是否终止程序
(defun menu-confrim-cancel (dcl_id / set-all-key-value mcc-action-tile->function mcc-run-function-by-dialog-id mcc-load-menu-window dcl_key_list dialog_return_id)
    ; 按钮动作处理
	(defun mcc-action-tile->function (recv_key)
		(cond
			((= recv_key "cancel") ; 取消按钮
				(done_dialog 0)
			)
			((= recv_key "accept") ; 确认按钮
				(done_dialog 1)
			)
		)
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun mcc-run-function-by-dialog-id (dcl_id /)
		(cond
			((= dcl_id 0)
              	(setq ConfirmCancelText_value "确认终止程序")
              	(start-main-menu dcl_id)
			)
			((= dcl_id 1)
              	(setq ConfirmCancel_value "确认终止程序")
				(term_dialog)
			)
		)
	)
    ; 加载确认取消对话框
	(defun mcc-load-menu-window (dcl_id / )
		(setq dialog_name "ConfirmCancel")
		(setq dcl_key_list '("ConfirmCancelText"))
        (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		(if (not (new_dialog dialog_name dcl_id)) (exit))
		(foreach key dcl_key_list
			(if (eval (read (strcat key "_value")))
				(set_tile key (eval (read (strcat key "_value"))))
				(set (read (strcat key "_value")) "确认终止程序")
			); 控件回显
			(action_tile key "(mcc-action-tile->function $key)"); 动作注册
		)
		(setq dialog_return_id (start_dialog))
        (unload_dialog dcl_id)
		(mcc-run-function-by-dialog-id dialog_return_id)
	)
  (mcc-load-menu-window dcl_id)
)

; 主菜单对话框 - DCL模板
(defun start-main-menu (dcl_id / smm-set-all-key-value smm-action-tile->function smm-run-function-by-dialog-id smm-load-menu-window dcl_key_list dialog_return_id)
    ; 设置所有已编辑控件的值到对应的全局变量
	(defun smm-set-all-key-value ( / dcl_key )
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)

    ; 全局按钮的动作处理
	(defun smm-action-tile->function (recv_key)
		(cond
			((= recv_key "cancel") ; 取消按钮
				(done_dialog 0)
			)
			((= recv_key "accept") ; 确认按钮
				(done_dialog 1)
			)
			((= recv_key "help") ; 帮助按钮
				(smm-set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "AddFeaturePoint") ; 添加特征点按钮
				(smm-set-all-key-value)
				(done_dialog 3)
			)
			((= recv_key "RepickFeaturePoint") ; 重新选择特征点按钮
				(smm-set-all-key-value)
				(done_dialog 4)
			)
			((= recv_key "DeleteLastPoint") ; 删除上一个点按钮
				(done_dialog 5)
			)
			((= recv_key "SelectSectionLine") ; 选择剖面线按钮
				(done_dialog 6)
			)
			((= recv_key "isSelectSectionLine") ; 选择平面图上的剖面线按钮
				(smm-set-all-key-value)
				(done_dialog 7)
			)
			((= recv_key "SelectSectionGround") ; 选择剖面地面线按钮
				(done_dialog 8)
			)
			((= recv_key "isSelectSectionGround") ; 选择剖面图上的剖面线按钮
				(smm-set-all-key-value)
				(done_dialog 9)
			)
			((= recv_key "PlanScale") ; 平面图比例
				(smm-set-all-key-value)
				(setq *plan_scale (atof PlanScale_value)); 平面图的比例
				; (done_dialog 10)
			)
			((= recv_key "SectionScale") ; 剖面图比例
				(smm-set-all-key-value)
  				(setq *section_scale (atof SectionScale_value)); 剖面图的比例
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
			((= recv_key "BottonForOutput") ; 导出按钮
				(smm-set-all-key-value)
				(done_dialog 15)
			)
		)
	)
    ; 根据dialog返回ID执行对应的业务逻辑
	(defun smm-run-function-by-dialog-id (recv_dialog_id / is_line)
		(cond
			((= recv_dialog_id 0) ; 取消
               (menu-confrim-cancel dcl_id)
			)
			((= recv_dialog_id 1) ; 确认
               (menu-confrim-end dcl_id)
			)
        	((= recv_dialog_id 3) ; 添加特征点
               (setvar "osmode" 14335)
               (setq *get_point (getpoint "选择点位："))
               (setvar "osmode" 0)
               (select-feature-point-type dcl_id)
			)
			((= recv_dialog_id 4) ; 重新选择特征点
               (setvar "osmode" 14335)
               (setq *get_point (getpoint "选择点位："))
               (setvar "osmode" 0)
               (select-feature-point-type dcl_id)
			)
			((= recv_dialog_id 5) ; 删除上一个点
               (deleta-last-point)
               (smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 6) ; 选择剖面线
			)
			( (= recv_dialog_id 7); 选择平面图上的剖面线
				(setq SelectSectionLine_value "已选择")
               	(setq is_line 0)
              	(while (= is_line 0)
					(setq *plan_line_in_plan (car (entsel "请选择平面图上的剖面线："))) ; 获得平面图上的剖面线段对象
       	 	(setq ent_type (cdr (assoc 0 (entget *plan_line_in_plan))))
           .if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
                (progn
							(setq is_line 1)
                .  (setvar "osmode" 14335)
   						(setq *plan_start_point (getpoint "选择起点："))
							(setq *plan_end_point (getpoint "选择终点："))
   						(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
                .  (setq *plan_line_handle (cdr (assoc 5 (entget *plan_line_in_plan)))); 剖面线段的handle值
   					  (setvar "osmode" 0)
						)
				 	)
				)
              	(smm-load-menu-window dcl_id)
			)
			((= recv_dialog_id 8); 选择剖面地面线
			)
			((= recv_dialog_id 9); 选择剖面图上的剖面线
               	(setq SelectSectionGround_value "已选择")
				(setq is_line 0)
				(while (= is_line 0)
       		(setq *section_line_ground (car (entsel "请选择剖面图上的剖面线："))) ; 获得平面图上的剖面线段对象
       	 	(setq ent_type (cdr (assoc 0 (entget *section_line_ground))))
           .  (if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
						(progn
							(setq is_line 1)
                .  (setvar "osmode" 14335)
   						(setq *section_start_point (getpoint "选择起点："))
							(setq *section_end_point (getpoint "选择终点："))
   						(setq *obj_section_line_ground (vlax-ename->vla-object *section_line_ground))
							(setq *section_line_handle (cdr (assoc 5 (entget *section_line_ground)))); 剖面线段的handle值
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
			((= recv_dialog_id 13); 导入/恢复数据的功能处理
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
				(progn ; 数据恢复功能
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
						(if *plan_line_handle ; 恢复平面图线
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
						(if *section_line_handle ; 恢复剖面图线
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
			((= recv_dialog_id 15); 导出/保存数据的功能处理
               (if (and IMPORT_FILE_PATH (!= IMPORT_FILE_PATH "NULL"))
				(princ)
                (setq IMPORT_FILE_PATH (getfiled "选择指导文件" "" "vlsd1" 48) )
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
    ; 导出全局变量值
	(defun smm-send-global-value ()
		;(setq draw_scale  (read drawingScaleInput_value))
	)
    ; 加载主菜单对话框
	(defun smm-load-menu-window (dcl_id / )
		(setq dialog_return_id 2 dialog_name "StartMenu")
		(setq dcl_key_list '("AddFeaturePoint" "DeleteLastPoint" "RepickFeaturePoint" "SelectSectionLine" "isSelectSectionLine" "SelectSectionGround" "isSelectSectionGround" "PlanScale" "SectionScale" "ImportBakupData" "BottonForImport" "OutputBakupData" "BottonForOutput"))
        (setq dcl_id (load_dialog DCL_PATH))
		(if (<= dcl_id 0)(exit))
		; (while (> dialog_return_id 1) ; dialogStatus 为1时表示当前按确认，0是按取消
			(if (not (new_dialog dialog_name dcl_id)) (exit))
			(foreach key dcl_key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				); 控件回显
				(action_tile key "(smm-action-tile->function $key)"); 动作注册
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
; (defun save-stratum-limits-point-value( / i ) ; "location_name" "soil_limits" "intense_limits" "moderate_limits"
;   (alert "start")
;   (setq curr_soil_limits_list (vls-string->list soil_limits_value ","))
; 	(setq count_soil (length curr_soil_limits_list))
;   (setq i 0)
;   (while (< i count_soil)
; 		(set (read (strcat "soil_limits_list_" (itoa i))) (append (eval (read (strcat "soil_limits_list_" (itoa i)))) (list (nth i curr_soil_limits_list)) ))
;     (setq i (1+ i))
;   )
; )


; 获取起点系数 - 计算所选线段上指定点的实际距离的绝对值
(defun get-start-point-coefficient (obj_selected_line selected_point / result)
  (setq result (abs (- (vla-get-Length obj_selected_line) (vlax-curve-getdistatpoint obj_selected_line selected_point) )))
  result
)

; 获取界限索引列表 - 对距离列表进行排序，得到按距离排序的索引列表{limits_dist_index_list}和第二个元素开始的列表{next_limits_index_list}
(defun get-limits-index-lists ( / temp_dist temp_dist)
	(foreach temp_point *limits_point_list
		(setq temp_dist (vlax-curve-getdistatpoint *obj_plan_line_in_plan temp_point))
		(setq temp_dist (abs (- temp_dist delta_start_point_dist)) )
		(setq limits_dist_list (append limits_dist_list (list temp_dist)))
	) ; 根据距离长度获取距离
	(setq *limits_dist_index_list (vl-sort-i limits_dist_list sign_comparator)); 距离排序索引
  (setq *next_limits_index_list (cdr *limits_dist_index_list))
)

; 创建多段线 - 根据点列表创建多段线对象
(defun make-polyline ( point_list / vertices result)
	(setq dxf_90  (cons 90 (length point_list)))
  (setq vertices (mapcar '(lambda (p) (cons 10 p)) point_list))
  ;; 端点列表
  (setq result (entmakex
    (append
      '((0 . "LWPOLYLINE")  ; 实体类型
        (100 . "AcDbEntity")
        (100 . "AcDbPolyline")
        dxf_90            ; 顶点数
        (70 . 0))           ; 闭合标志：0 表示开放
      vertices
    )
  ))
  result
)

; 创建剖面线 - 根据坐标创建垂直剖面线
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

; 获取线上点 - 根据x坐标值返回与多段线的交点坐标
(defun get-point-at-line (obj_pline recv_x recv_line / obj_line obj_intersection_points intersection_points )
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

; 获取绘制界限列表 - 根据距离列表计算各地层的绘制坐标值
(defun get-draw-limits-lists ( / )
    ; 定义内部函数：输入绘制线的值
	(defun input-draw-line-value (recv_coor_x recv_coor_y recv_soil_limits recv_intense recv_moderate / index_number )
		(setq index_number  0)
		(while (< index_number (length recv_soil_limits)) ; 逐点赋值
			; {draw_soil_line_poiont_list}; {draw_soil_line_poiont}
			(setq recv_coor_y (- recv_coor_y (nth index_number recv_soil_limits) ))
			(set (read (strcat "*draw_soil_line_" (itoa index_number))) (append (read (strcat "*draw_soil_line_" (itoa index_number))) (list recv_coor_x recv_coor_y 0))); 土壤层分界点列表
		)
		(setq recv_coor_y (- recv_coor_y recv_intense)); 强度界限递减
		(setq *draw_intense_line (append *draw_intense_line (list recv_coor_x recv_coor_y 0))); 强度界限点串
		(setq recv_coor_y (- recv_coor_y recv_moderate)); 缓动界限递减
		(setq *draw_moderate_line (append *draw_moderate_line (list recv_coor_x recv_coor_y 0))); 缓动界限点串
	)
    ; 添加绘制界限值 - 根据两个相邻界限点的数据计算中间值
  (defun add-draw-limits-value (index_1st index_2nd / second_soil_limits first_soil_limits)
		(setq first_index_number (nth index_1st *limits_dist_index_list))
		(setq second_index_number (nth index_2nd *next_limits_index_list))
		(setq first_soils  (nth first_index_number *soil_limits_list)); 前一个点的土层值
  		(setq first_soil_list_str (vls-string->list first_soils ",")); 转换为列表
        (setq first_soil_limits '())
		(foreach soil_str first_soil_list_str
			(setq first_soil_limits (append first_soil_limits (list (atof soil_str))))
    ); 实现了字符串到数值列表的转换 {first_soil_limits}
		(setq second_soils  (nth second_index_number *soil_limits_list)); 下一个点的土层值
        (setq second_soil_list_str (vls-string->list second_soils ",")); 转换为列表
        (setq second_soil_limits '())
		(foreach soil_str second_soil_list_str
			(setq second_soil_limits (append second_soil_limits (list (atof soil_str))))
    ); 实现了字符串到数值列表的转换 {second_soil_limits}
		(setq first_intense  (atof (nth first_index_number *intense_limits_list))); 强度界限数值
		(setq second_intense  (atof (nth second_index_number *intense_limits_list))); 强度界限数值
		(setq first_moderate  (atof (nth first_index_number *moderate_limits_list))); 缓动界限数值
		(setq second_moderate  (atof (nth second_index_number *moderate_limits_list))); 缓动界限数值

		(setq x_increase_dist  (* (atof (nth first_index_number limits_dist_list)) x_scale) ); 前一个点的距离值，x的加值
		(cond
			((> x_increase_dist 2000) (setq insert_number (1- (/ (fix x_increase_dist) 100))) )
			((> x_increase_dist 1000) (setq insert_number (1- (/ (fix x_increase_dist) 30 ))) )
			((> x_increase_dist 100 ) (setq insert_number (1- (/ (fix x_increase_dist) 15 ))) )
			((> x_increase_dist 10  ) (setq insert_number (1- (/ (fix x_increase_dist) 5  ))) )
			(T (setq insert_number 2) )
    ); 根据插值点x距离长度判断, {insert_number}
		(setq x_increment (/ x_increase_dist insert_number)); 插值点,每个x的增量距离
		(setq curr_soils_length (length second_soil_limits))

        (setq j  0)
      	(while (< j curr_soils_length)
			(set (read (strcat "first_soil_" (itoa j))) (nth j first_soil_limits) )
			(set (read (strcat "second_soil_" (itoa j))) (nth j second_soil_limits) )
      .  (set (read (strcat "soil_increment_" (itoa j) )) (/ (* (- (nth j second_soil_limits) (nth j first_soil_limits)) y_scale) (1+ y_scale)) )
			(setq j  (1+ j))
      ); 计算各层间的增量y值 {soil_increment_[层号]}
      	(setq intense_increment (/ (* (- second_intense first_intense) y_scale) (1+ insert_number)) ); 强度增量值 {intense_increment}
      	(setq moderate_increment (/ (* (- second_moderate first_moderate) y_scale) (1+ insert_number)) ); 缓动增量值 {moderate_increment}
		(setq temp_coor_x section_start_x_coordinate); 记录当前的X坐标
		(setq section_start_x_coordinate (+ section_start_x_coordinate x_increase_dist)); 完成一段线到下一点，作为下一循环的开始

		(setq curr_coor_x temp_coor_x); 当前X
		(setq curr_soil_limits first_soil_limits)
		(setq curr_intense first_intense)
     	(setq curr_moderate second_moderate)
		(while (< curr_coor_x section_start_x_coordinate) ; 插值点列表
			(setq curr_coor_y (cadr (get-point-at-line *obj_section_line_ground curr_coor_x intersectionwith_line)) ); 获取地面值的点坐标
			(input-draw-line-value curr_coor_x curr_coor_y curr_soil_limits curr_intense curr_moderate)
            ; ---fold---
			(vl-cmdf "move" intersectionwith_line "" temp_move_point  x_increment)
			(setq curr_coor_x (+ x_increment curr_coor_x)); 增量X
		)
		; 准备好下一个循环的下一个距离值

       	(entdel intersectionwith_line)
		(setq total_x_increment (+ total_x_increment x_increase_dist))
		(vl-cmdf "copy" at_start_point_line "" 0 total_x_increment)
		(setq intersectionwith_line (entlast))
		(setq i (1+ i))
	)
    ; 启动获取绘制界限列表
    (defun start-get-draw-limits-lists ( / )
		(setq length_list (length *next_limits_index_list) i 0)
		(setq temp_coor_y (cadr *section_start_point)); 记录初始值用于计算Y偏移
		(setq section_start_x_coordinate (car *section_start_point))
		(setq total_x_increment 0); 总增量 用于复制初始点线 {at_start_point_line} 和 {intersectionwith_line}
		(setq temp_move_point (list section_start_x_coordinate temp_coor_y)); 移动的起点
		(setq at_start_point_line (make-sectionwith-line section_start_x_coordinate temp_coor_y)); 创建一条垂直于y值的线
		(vl-cmdf "copy" at_start_point_line "" 0 0)
		(setq intersectionwith_line (entlast)); 复制线段的目标要接近的里程处理
		; --插值赋值循环
		(while (< i length_list)
			(add-draw-limits-value i i)
			(setq i (1+ i))
		)
		; 处理最后一个点值
		(progn ; 处理最后一个点值
			(setq curr_coor_x (car *section_end_point  )); 当前X
			(setq curr_coor_y (cadr *section_end_point)); 当前X
			(setq first_soils  (nth i *soil_limits_list)); 前一个点的土层值
			(setq curr_soil_limits_str (vls-string->list first_soils ",")); 转换为列表
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

; 绘制土壤层线 - 生成各地层的分界线
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



; 初始化变量 - 设置DCL路径和全局变量
(defun init-variant ()
  (setq DCL_PATH "W:\\Plugin\\LISP\\AutoGeoDraw\\AutoGeologicalProfilePlot\\AGPPMenu.dcl")
  (setq dcl_id 999)
  (setq *saved_variants '("*plan_start_point" "*plan_end_point" "*plan_line_handle" "*section_start_point" "*section_end_point" "*section_line_handle" "*location_name_list" "*soil_limits_list" "*intense_limits_list" "*moderate_limits_list" "*limits_point_list" "*structure_point_type_list" "*structure_point_name_list" "*last_geo_code_list" "*next_geo_code_list" "*structure_point_list" "*plan_scale" "*section_scale" "*data_saved_path" "*data_import_path" "IMPORT_FILE_PATH"))
  (setq IMPORT_FILE_PATH '())
  (setq OUTPUT_FILE_PATH '())
  ; ----fold---
		(setq structure_point_type 3) ; 3为边界点 4为断裂 5为褶皱
)


; 设置变量 - 计算比例尺和相关参数
(defun set-variant ( / )
  (setq *plan_scale (atof PlanScale_value)); 平面图的比例
  (setq *section_scale (atof SectionScale_value)); 剖面图的比例
  (setq x_scale (/ *plan_scale *section_scale)); 横坐标比例
  (setq y_scale (/ 1000 *section_scale)); 纵坐标比例
	(setq delta_start_point_dist (get-start-point-coefficient *obj_plan_line_in_plan *plan_start_point)); 起始点的差值，用于计算相对于起始点在线段上的位置才能不重复
  (setq section_start_x_coordinate (car *section_start_point))
  (if (> delta_start_point_dist 0) ; 判断起点方向正确性
		(setq sign_comparator (quote >))
		(setq sign_comparator (quote <))
	)
	(get-limits-index-lists); {limits_dist_list}, {limits_dist_index_list}
  (get-draw-limits-lists);
)

; 打印变量 - 调试用，输出变量列表
(defun print-variant (variant_list / i )
  (setq list_length (length variant_list))
  (foreach var variant_list)
	(princ (strcat (read var) "is:" var "\n"))
  (princ)
)




;-------------------------
; 主程序入口 - 启动地质剖面绘图程序
(defun start-program ( / )
  (setq test_bakup_data '("*plan_line_handle:STR:2CEE" "*section_line_handle:STR:2CF0" "*location_name_list:LIST:(pt1 pt2 pt2 11 12)" "*soil_limits_list:LIST:(3,4,5 3,4,6 3,4,3 3,4,3 3,4,3)" "*intense_limits_list:LIST:(2 3 4 4 4)" "*moderate_limits_list:LIST:(4 7 5 5 5)" "*limits_point_list:LIST:((307.961 1647.39 0.0) (509.677 1849.11 0.0) (942.09 2281.52 0.0) (1194.5 2533.93 0.0) (1418.96 2758.39 0.0))" "*structure_point_type_list:LIST:(3 4 5)" "*structure_point_name_list:LIST:(pt1 pt2 4)" "*last_geo_code_list:LIST:(1 3 5)" "*next_geo_code_list:LIST:(2 4 6)" "*structure_point_list:LIST:((523.755 1863.19 0.0) (736.945 2076.38 0.0) (1181.43 2520.86 0.0))" "*plan_scale:STR:50000.0" "*section_scale:STR:500.0" "*data_saved_path:STR:nil" "*data_import_path:STR:nil" "recover_status:STR:OK"))
	(init-variant)
	(start-main-menu dcl_id)
	(set-variant)
  (alert "start_drawing")
  (draw-soil-line)
  (print-variant)
)


; 测试函数 - 用于测试数据恢复功能
(defun c:test( / )
  (alert "vmodify: 2024-09-13 00:00")
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
				(progn ; 数据恢复功能
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
						(if *plan_line_handle ; 恢复平面图线
							(progn
								(setq *plan_line_in_plan (handent *plan_line_handle))
          				(setq ent_type (cdr (assoc 0 (entget *plan_line_in_plan))) )
								(if (or (equal ent_type "LWPOLYLINE") (equal ent_type "LINE"))
									(setq *obj_plan_line_in_plan (vlax-ename->vla-object *plan_line_in_plan))
									(setq SelectSectionLine_value "已选择")
								)
                )
						)
						(if *section_line_handle ; 恢复剖面图线
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
	)
)






; 主程序
(defun C:TT (/ *error* *CurDoc* *Old_SysVar*) ; -> 主程序入口
	(setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
	(defun *error* (msg)  ; 错误处理
		; 主要用于恢复设置