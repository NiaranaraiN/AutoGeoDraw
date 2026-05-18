;--------------------------------
	(if (null vlax-dump-object) (vl-load-com));将Visual LISP扩展功能加载到 AutoLISP
	(load "TbxlsCore")
;--------------------------------
; 保存程序运行前环境状态
(defun bakup_env()
	(setq osmode_ctrl (getvar "osmode")) ;保存捕捉状??
	(setq clayer_ctrl (getvar "clayer")) ;保存图层状??
);finished

;恢复程序前的状??
(defun recover_env()
  	(setvar "osmode" osmode_ctrl)
  	(setvar "clayer" clayer_ctrl)	
)

;获得多段线x坐标
(defun get_coorx( pline / start_point end_point pline_coorx)
		;提取起点及终点的X坐标仩值
			(setq start_point (vlax-curve-getstartpoint pline))
			(setq start_point (car start_point))
			(setq end_point (vlax-curve-getendpoint pline))
			(setq end_point (car end_point))
		;坐标值从小到大排列
			(if (< start_point end_point)
				(setq pline_coorx (list start_point end_point))
				(setq pline_coorx (list end_point start_point))
      )
			pline_coorx
)


;获取给定曲线的x坐标，并组成集合pline_coorx
(defun get_coorx_list(pline_set / pline_index set_length result) "获取给定曲线的x坐标，并组成集合pline_coorx" "多线起点和终点坐标的列表"
	(setq pline_index 0 result '())
  	(if pline_set
		(progn
			(setq set_length (sslength pline_set))
			(while (< pline_index set_length)
				(setq pline_coorx_result (get_coorx (ssname pline_set pline_index)))
				(setq result (append result (list pline_coorx_result)))
				(setq pline_index (+ 1 pline_index))
			)
    )
			(setq result '((0 0)))
  )
	result
);返回坐标值集合

;提取线段选择集内的起点x坐标值
(defun cuntain_start_point_coordinate (curtain_line_set get_method / coordinate_list item_coordinate item_index set_length result )
	"提取线段选择集内的起点坐标值,
	如果'get_method'为'X'则提取X坐标值，
	如果'get_method'为'Y'则提取Y坐标值，
	" 
	"@returns 返回一个列表，包含所有X或Y坐标值"
	;---
		(setq set_length (sslength curtain_line_set))
		(setq  item_index 0);初始化索引号
		(setq coordinate_list '());初始化坐标列表
		(while (< item_index set_length)
			(setq item (ssname curtain_line_set item_index));获取单条帷幕灌浆线
			(cond
				((= get_method "X_VALUE") (setq item_coordinate (car (vlax-curve-getstartpoint item))));如果是X，则选取x值
				((= get_method "Y_VALUE") (setq item_coordinate (cadr (vlax-curve-getstartpoint item))));如果不是X，提取Y值
			);获取线段x坐标值)
			(setq coordinate_list (append coordinate_list (list item_coordinate)))
			(setq item_index (1+ item_index))
		)
		(setq result coordinate_list)
		result
		;对列表索引排序得到列表result_list：(setq result_list (vl-sort-i list '<))
)

;计算缩放比例后的数据，有一个需求数据raw_data，有参照数据consult_data，参照数据对应的实际数据consult_actual_value，缩放比例actual_scale,求得raw_actual_value(即result)
(defun caculate_real_data (raw_data consult_data consult_actual_value actual_scale / result )  "输入需要计算点的Y值、参照点的Y值、参照点实际坐标值及图纸比例，计算出计算点的高程值" "真实高程值" 
	(setq result (+ consult_actual_value (* (- raw_data consult_data) (/ actual_scale 1000) ) ) )
  (setq result (read (rtos result 2 *result_precison*)))
  result
)
                                   
;求两个布线的交点。并返回交点坐标值（本程序中帷幕灌浆与各界限交点仅有0~1个值，所以不考虑多个交点的问题。
(defun get_inter_point (pline1 pline2 / obj_inter_result result obj_1 obj_2) "求两个布线的交点。并返回交点坐标值（本程序中帷幕灌浆与各界限交点仅有0~1个值，所以不考虑多个交点的问题。" "返回交点坐标，列表型"
  (setq obj_1 (vlax-ename->vla-object pline1))
  (setq obj_2 (vlax-ename->vla-object pline2))
  (setq obj_inter_result (vlax-variant-value (vlax-invoke-method obj_1 'IntersectWith obj_2 0)))
  (setq result (vlax-safearray->list obj_inter_result))
  result
)
 

;求一条多段线与一组多段线是否存在交点，如果存在，取得该点的坐标值
(defun stratum_inter (curtain_bore curtain_bore_coorx pline_set pline_set_coorx_list / inter_point stratum_line pline_coorx_min pline_coorx_max set_length set_index result) "求一条多段线与一组多段线是否存在交点，如果存在，取得该点的坐标值" "点坐标值"
	(setq set_length (sslength pline_set))
	(setq set_index 0 result '(0 0 0))
	(while (< set_index set_length)
    (setq pline_coorx_min (car (nth set_index pline_set_coorx_list)));多段线组起点X
    (setq pline_coorx_max (cadr (nth  set_index pline_set_coorx_list)));多段线组终点X
    (if (and (>= curtain_bore_coorx pline_coorx_min) (< curtain_bore_coorx pline_coorx_max));如果为真，说明钻孔位于该线段内
        ;为真：
        (progn 
					(setq stratum_line (ssname pline_set set_index));获取地层类线集内的索引对应多段线{stratum_line}
					(setq inter_point (get_inter_point curtain_bore stratum_line ));取得坐标值
					(setq result inter_point)
					(setq set_index (1+ set_length));跳出循环
        )
        ;为假
        (setq set_index (1+ set_index))
    )
  )
  result
)
;resule*
;取得地层类进尺深度，返回真实长度
(defun stratum_length (curtain_bore curtain_bore_coorx delta_elevation pline_set pline_set_coorx_list bore_top_elevation water_actual_elevation draw_scale / arg1 arg2 arg3 arg4 stratum_inter_point stratum_inter_coorx stratum_inter_coory stratum_elevation result)
  (setq arg1 curtain_bore arg2 curtain_bore_coorx arg3 pline_set arg4 pline_set_coorx_list)
  (if arg3  ;如果选择集存在
  (progn
    ; (setq stratum_inter_point (stratum_inter curtain_bore curtain_bore_coorx pline_set pline_set_coorx_list));求得交点坐标
    (setq stratum_inter_point (stratum_inter arg1 arg2 arg3 arg4));求得交点坐标
    (setq stratum_inter_coorx (nth 0 stratum_inter_point))
    (setq stratum_inter_coory (nth 1 stratum_inter_point))
    (if (and (= stratum_inter_coorx 0) (= stratum_inter_coory 0))
      (setq result 0)
      (progn
        (setq stratum_elevation (caculate_real_data stratum_inter_coory delta_elevation water_actual_elevation draw_scale))
        (setq result (- bore_top_elevation stratum_elevation))
      )
    )
    
    (setq result (read (rtos result 2 *result_precison*)))
  )
    (setq result 0)
  )
  result
)

;去除土体、坝体、砼进尺同时存在时上部重复的部分
(defun remove_overlap_length ( arg1 arg2 arg3 / length_list list_index_list value_max value_mid value_min remove_overlap_list i item_value item result)
  "去除土体、坝体、砼进尺同时存在时上部重复的部分，strtum_length计算的是从孔口至层底限,所以如果同时存在，上部分有重复的。思路则是三者对比排序，最大-次大，次大-最小，得到最新的数据" 
  ""
  (setq length_list (list arg1 arg2 arg3))
  (setq list_index_list (vl-sort-i length_list '>));从大到小排列{length_list}的顺序
  (setq value_max (nth (nth 0  list_index_list) length_list ))
  (setq value_mid (nth (nth 1 list_index_list) length_list))
  (setq value_min (nth (nth 2 list_index_list) length_list))
  (setq value_max (- value_max value_mid));最大值去重
  (setq value_mid (- value_mid value_min));中间值去重
  (setq remove_overlap_list (list value_max value_mid value_min));按{list_index_list}索引得到的列表
  (setq i 0 result '(99990 99991 99992));索引号+99990，以防止短进尺偶尔重复
  (foreach item list_index_list
    (setq item_value (nth i remove_overlap_list))
    (setq result (subst item_value (+ 99990 item) result))
    (setq i (1+ i))
	)
  result
)
	


;获取输入数值属性
(defun print_value_prop (get_value / ) "函数功能说明" "函数返回值"
      (princ "\n类型：")
      (princ (type get_value))
      (princ "\n数值：")
      (princ (rtos get_value))
      (princ "\n ------------")
      (princ "\n")
  )

;-------------------------------------------------------------------

(defun test( / result)
  (setq result (remove_overlap_length 12 10 13))
  (princ "\n------")
  (princ result)
  (princ "\n------")
)


;;运行程序
(defun start_program ()
  (defun action-format-keys (key) ;全部控件的点击动作触发
    (cond
      ((= key "accept") ;{确认按钮}
        (key-value-bak)
        (done_dialog 1);对话框退出返回主函数 传递给Dialog_Return值为1
      )
      ((= key "cancel") ;{取消按钮}
        (done_dialog 0);对话框退出返回主函数 传递给Dialog_Return值为0
      )
      ((= key "SSCurtainButtom") ; 选择灌浆线
        (key-value-bak)
        (done_dialog 3)
      )
      ((= key "SelectCurtainStatus") ; 灌浆线选择情况
        ()
      )
      ((= key "SSWaterLevelButtom") ; 选择正常蓄水位线
        (key-value-bak) 
        (done_dialog 4)
      )
      ((= key "drawingScaleInput") ; 图纸比例
        ()
      )
      ((= key "WaterLevelElevationInput") ; 正常蓄水位高程输入框
        ()
      )
      ((= key "SSGroundButtom") ; 选择地面线
        (key-value-bak)
        (done_dialog 5)
      )
      ((= key "SelectGroundStatus") ; 地面线选择状态
        ()
      )
      ((= key "SSSoilButtom") ; 选择土底限
        (key-value-bak)
        (done_dialog 6)
      )
      ((= key "SelectSoilStatus") ; 选择土底限状态
        ()
      )
      ((= key "SSDamTopButtom") ; 选择坝顶线
        (key-value-bak)
        (done_dialog 7)
      )
      ((= key "SelectDamTopStatus") ; 选择坝顶线状态
        ()
      )
      ((= key "SSDamBottomButtom") ; 选择坝底线
        (key-value-bak)
        (done_dialog 8)
      )
      ((= key "SelectDamBottomStatus") ; 选择坝底线状态
        ()
      )
      ((= key "SSConcreteButtom") ; 选择混凝土底限（其它进尺线）
        (key-value-bak)
        (done_dialog 9)
      )
      ((= key "SelectConcreteStatus") ; 其它进尺线选择状态
        ()
      )
      ((= key "SelectResetButtom") ; 选择初始化程序
        (key-value-bak)
        (done_dialog 10)
      )
      ((= key "SavePathButtom") ; 选择保存位置
        (key-value-bak)
        (done_dialog 11)
      )
      ((= key "ShowSavePath") ; 显示保存位置
        ()
      )
    )
  ) 

  (defun key-value-bak ( / key ) ;保存已经输入的值
    (foreach key keyList
      (set (read (strcat key "_value")) (get_tile key))
    )
    
  )

  (defun botton-action (bottonId / ss_length)
    (cond 
      ( (= bottonId 3);给帷幕灌浆线赋值
        (princ "\n 选择帷幕灌浆线:")
        (setq ss_curtain (ssget))
        (setq ss_length (sslength ss_curtain))
        (if (= ss_length 0)
          (setq SelectCurtainStatus_value "无选择")
          (setq SelectCurtainStatus_value (strcat "已选" (itoa ss_length)))
        )
      )

      ( (= bottonId 4) 
        (princ "\n 选择正常蓄水位线:")
        (setq ss_water_level (ssget))
        (setq ss_length (sslength ss_water_level))
        (if (= ss_length 0)
          (setq SelectWaterLevelStatus_value "无选择")
          (setq SelectWaterLevelStatus_value (strcat "已选" (itoa ss_length)))
        )
      )


      ((= bottonId 5)
        (princ "\n 选择地表面线:")
        (setq ss_ground (ssget))
        (setq ss_length (sslength ss_ground))
        (if (= ss_length 0)
          (setq SelectGroundStatus_value "无选择")
          (setq SelectGroundStatus_value (strcat "已选" (itoa ss_length)))
        )      
      )


      ((= bottonId 6)
        (princ "\n 选择覆盖层与基岩分界线:")
        (setq ss_soil (ssget))
        (setq ss_length (sslength ss_soil))
        (if (= ss_length 0)
          (setq SelectSoilStatus_value "无选择")
          (setq SelectSoilStatus_value (strcat "已选" (itoa ss_length)))
        )     
      )


      ((= bottonId 7)
        (princ "\n 选择坝顶线:")
        (setq ss_dam_top (ssget))
        (setq ss_length (sslength ss_dam_top))
        (if (= ss_length 0)
          (setq SelectDamTopStatus_value "无选择")
          (setq SelectDamTopStatus_value (strcat "已选" (itoa ss_length)))
        )        
      )


      ((= bottonId 8)
        (princ "\n 选择坝底线:")
        (setq ss_dam_bottom (ssget))
        (setq ss_length (sslength ss_dam_bottom))
        (if (= ss_length 0)
          (setq SelectDamBottomStatus_value "无选择")
          (setq SelectDamBottomStatus_value (strcat "已选" (itoa ss_length)))
        )            
      )


      ((= bottonId 9)
        (princ "\n 选择其它分界线:")
        (setq ss_concrete (ssget))
        (setq ss_length (sslength ss_concrete))
        (if (= ss_length 0)
          (setq SelectConcreteStatus_value "无选择")
          (setq SelectConcreteStatus_value (strcat "已选" (itoa ss_length)))
        )                   
      )


      ((= bottonId 10) ;重置
        (setq SelectCurtainStatus_value "无选择")
        (setq SelectWaterLevelStatus_value "无选择")
        (setq SSelectGroundStatus_value "无选择")
        (setq SelectSoilStatus_value "无选择")
        (setq SelectDamTopStatus_value "无选择")
        (setq SelectDamBottomStatus_value "无选择")
        (setq SelectConcreteStatus_value "无选择")
        (setq WaterLevelElevationInput_value "")
        (setq drawingScaleInput_value "")
        (setq ShowSavePath_value "C:\\")
        (setq dialogStatus 2 ss_curtain nil ss_ground nil ss_dam_top nil ss_dam_bottom nil ss_water_level nil ss_soil nil ss_concrete nil ) 
      )


      ((= bottonId 11)
        (setq SAVE_PATH (getfiled "选择数据保存位置" ShowSavePath_value "csv" 1))
        (setq ShowSavePath_value SAVE_PATH)
      )
    )
    

  )

  (defun get-input-value ()
    (setq draw_scale  drawingScaleInput_value)
    (setq water_elevation  WaterLevelElevationInput_value)
    (setq water_elevation  WaterLevelElevationInput_value)
    (setq SAVE_PATH  ShowSavePath_value)

    
  )

  (defun load-menu ()
    (setq dialogFile "w:\\plugin\\lisp\\testDCL")
    (setq dialogName "MainMenu")
    (if (= dialogName "") (exit))
    (setq dialogId (load_dialog dialogFile))
    (if (< dialogId 0) (exit))
    (setq dialogStatus 2 ss_curtain nil ss_ground nil ss_dam_top nil ss_dam_bottom nil ss_water_level nil ss_soil nil ss_concrete nil )
    (setq keyList '("SSCurtainButtom" "SelectCurtainStatus" "SSWaterLevelButtom" "SelectWaterLevelStatus" "drawingScaleInput" "WaterLevelElevationInput" "SSGroundButtom" "SelectGroundStatus" "SSSoilButtom" "SelectSoilStatus" "SSDamTopButtom" "SelectDamTopStatus" "SSDamBottomButtom" "SelectDamBottomStatus" "SSConcreteButtom" "SelectConcreteStatus" "SelectResetButtom" "SavePathButtom" "ShowSavePath" "accept" "cancel"))
    (while (> dialogStatus 1)
      (if (not (new_dialog dialogName dialogId)) (exit))
      ;(new_dialog dialogName dialogId)


      (foreach key keyList
        (if (eval (read (strcat key "_value")))
          (set_tile key (eval (read (strcat key "_value"))))
        );控件内容
        (action_tile key "(action-format-keys $key)");点击动作
      )



      (setq dialogStatus (start_dialog))

      (botton-action dialogstatus)
      ; (cond
      ; 	((= dialogStatus 3)
      ;     ; (initget 1)
      ; 		(princ "选择帷幕灌浆线")
      ; 		(setq ss_curtain (ssget))
      ; 		(setq ss_length (length ss_curtain))
      ; 		(if (= ss_length 0)
      ; 			(setq SelectCurtainStatus_value "无")
      ; 			(setq SelectCurtainStatus_value "已选择")
      ; 		)
      ; 	)
      ; )
    )
    (unload_dialog dialogId)
  )

	;赋初值
    (load-menu)
    (get-input-value)
    ;重要，设置小数位数为3位！要改精度直接改这个值！
    (setq *result_precison* 3)
  
    ;图形比例设置，默认为1000
		; (setq draw_scale (getreal "图形比例:<1000>"));设置图形比例
		; (if (not (numberp draw_scale))
		; 		(setq draw_scale 1000)
    ; );设置图形比例

		; ;设置正常蓄水位高程
		; (setq water_elevation (getreal "请输入正常蓄水位高程："))
		; (while (not (numberp water_elevation)) 
    ; 	(setq water_elevation (getreal "请输入正常蓄水位高程："))
    ; );获取正常蓄水位高程值)
		

		; ;帷幕灌浆线合集
		; (alert "选择帷幕灌浆线")
		; (setq ss_curtain (ssget))

		; ;地面线合集
		; (alert "选择地面线")
		; (setq ss_ground (ssget))

		; ;;定义坝体底限合集
 		; (setq dam_exist (getstring "\n是否有坝体进尺？Y/N?:<Y>"))
  	; (while (and (/= dam_exist "Y") (/= dam_exist "y")(/= dam_exist "N") (/= dam_exist "n")) 
		; 		(setq dam_exist (getstring "\n是否有坝体进尺？Y/N?:<Y>")))
		; (if (or (= "Y" dam_exist) (= "y" dam_exist))
		; 		(progn
		; 			(alert "选择坝顶线：")
		; 			(setq ss_dam_top (ssget))
		; 			(alert "选择坝底限")
		; 			(setq ss_dam_bottom (ssget))
		; 		)
    ;     (progn 
    ;       (setq ss_dam_top nil)
    ;       (setq ss_dam_bottom nil)
    ;     )
    ; )
		
		; ;选择正常蓄水位线合集
		; (alert "选择正常蓄水位线")
		; (setq ss_water_level (ssget));选择正常蓄水位)
		(setq delta_elevation (cuntain_start_point_coordinate ss_water_level "Y_VALUE"))
		(setq delta_elevation (car delta_elevation));获取正常蓄水位的Y坐标值,以调用函数获得孔口高程及孔底高程

		; ;;定义土层线选择集
  	; (setq soil_exist (getstring "\n是否有土层？Y/N?:<Y>"))
		; (while (and (/= soil_exist "Y") (/= soil_exist "y")(/= soil_exist "N") (/= soil_exist "n")) 
		; 	(setq soil_exist (getstring "\n是否有土层？Y/N?:<Y>"))
    ; )
		; (if (or (= "Y" soil_exist) (= "y" soil_exist) (= soil_exist nil))
		; 		(progn
		; 			(alert "选择土层底限")
		; 			(setq ss_soil (ssget))
		; 		)
    ;     (setq ss_soil nil)
    ; )


		; ;;定义其它砼底限合集
		; (setq concrete_exist (getstring "\n是否有其它砼进尺？Y/N?:<Y>"))
		; (while (and (/= concrete_exist "Y") (/= concrete_exist "y")(/= concrete_exist "N") (/= concrete_exist "n"))
		; 	(setq concrete_exist (getstring "\n是否有其它砼进尺？Y/N?:<Y>"))
    ; )
		; (if (or (= "Y" concrete_exist) (= "y" concrete_exist))
		; 		(progn
		; 			(alert "选择砼底限")
		; 			(setq ss_concrete (ssget))
		; 		)
    ;     (setq ss_concrete nil)
    ; )
	
	; ;选择文件保存位置及名称
	; (setq SAVE_PATH (getfiled "选择数据保存位置" "w:\\" "csv" 1))
	(setq csv_file (open SAVE_PATH "w"))
	(write-line "钻孔编号,孔口高程,孔底高程,孔深,土层进尺,坝体进尺,砼进尺,基岩进尺,有效进尺,无效进尺" csv_file) 
  
;提取各线段x坐标起始值,均为列表
	(setq ground_coorx_list (get_coorx_list ss_ground));地面线x值列表
	(setq soil_coorx_list (get_coorx_list ss_soil));土层线x值列表
	; (setq dan_top_coorx_list (get_coorx_list ss_dam_top));坝顶x值列表
	(setq dam_coorx_list (get_coorx_list ss_dam_bottom));坝底x值列表
	(setq concrete_coorx_list (get_coorx_list ss_concrete));;其它砼x值列表

;提取帷幕灌浆线的起点坐标列表{curtain_coorx_list}及沿X方向布置的索引值{curtain_sorted_index}
	(setq curtain_coorx_list (cuntain_start_point_coordinate ss_curtain "X_VALUE"));获取帷幕灌浆线起点x坐标
	(setq curtain_coory_list (cuntain_start_point_coordinate ss_curtain "Y_VALUE"));获取帷幕灌浆线起点y坐标
	(setq curtain_sorted_index (vl-sort-i curtain_coorx_list '<));对curtain_coorx_list坐标值的索引值排序，得到帷幕灌浆从小到大的索引列表curtain_sorted_index

  
;对每个灌浆线进行处理
	(setq bore_number 1);设定钻孔编号值
	(foreach curtain_index curtain_sorted_index
		;单孔数值定义
			(setq curtain_bore (ssname ss_curtain curtain_index));提取帷幕灌浆线图元
			(setq curtain_top_coorx (nth curtain_index curtain_coorx_list));孔口X坐标
			(setq curtain_top_coory (nth curtain_index curtain_coory_list));孔口Y坐标
			(setq curtain_bottom_coory (nth 1 (vlax-curve-getEndPoint curtain_bore)));孔底Y坐标
      (if (< curtain_top_coory curtain_bottom_coory)
          (progn 
            (setq tmp_value curtain_top_coory)   
            (setq curtain_top_coory curtain_bottom_coory)   
            (setq curtain_bottom_coory tmp_value)   
          )
      )
		;计算各字段值
			(setq bore_top_elevation (caculate_real_data curtain_top_coory delta_elevation water_elevation draw_scale));求孔口高程{bore_top_elevation}
  		(setq bore_bottom_elevation (caculate_real_data curtain_bottom_coory delta_elevation water_elevation draw_scale));求孔底高程{bore_bottome_elevation}
      (setq bore_length (read (rtos (- bore_top_elevation bore_bottom_elevation) 2 *result_precison*)));求孔深{bore_length}
			(setq soil_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_soil soil_coorx_list bore_top_elevation water_elevation draw_scale));求土体进尺
			(setq dam_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_dam_bottom dam_coorx_list bore_top_elevation water_elevation draw_scale));求坝体进尺 
			(setq concrete_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_concrete concrete_coorx_list bore_top_elevation water_elevation draw_scale));求砼进尺
			;对土体进尺、坝体进尺、砼进尺进行去重处理
			(setq list_remoe_overlap (remove_overlap_length soil_length dam_length concrete_length))
			(setq soil_length (read (rtos (nth 0 list_remoe_overlap) 2 *result_precison*)));去重后的土体进尺
			(setq dam_length (read (rtos (nth 1 list_remoe_overlap) 2 *result_precison*)));去重后的坝体进尺 
			(setq concrete_length (read (rtos (nth 2 list_remoe_overlap) 2 *result_precison*)));去重后的砼进尺
   
			(setq bedrock_length (read (rtos (- bore_length soil_length dam_length concrete_length) 2 *result_precison*)));求基岩进尺
			(setq invalid_length (- bore_top_elevation water_elevation));无效进尺
			(if (< invalid_length 0) (setq invalid_length 0))
      (setq invalid_length (read (rtos invalid_length 2 *result_precison*)))
			(setq effective_length (read (rtos (- bore_length invalid_length dam_length concrete_length) 2 *result_precison*)));有效进尺
			;组合：钻孔编号,孔口高程,孔底高程,孔深,土层进尺,坝体进尺,砼进尺,基岩进尺,有效进尺,无效进尺
			(setq strcat_to_line 
        (strcat (rtos bore_number 2 *result_precison*) "," (rtos bore_top_elevation 2 *result_precison*) "," (rtos bore_bottom_elevation 2 *result_precison*) "," (rtos bore_length 2 *result_precison*) "," (rtos soil_length 2 *result_precison*) "," (rtos dam_length 2 *result_precison*) "," (rtos concrete_length 2 *result_precison*) "," (rtos bedrock_length 2 *result_precison*) "," (rtos effective_length 2 *result_precison*) "," (rtos invalid_length 2 *result_precison*)
        )
      )
			(write-line strcat_to_line csv_file)
			(setq bore_number (1+ bore_number))
  	)
;关闭CSV
	(close csv_file)
  (alert (strcat "进尺统计完成！已保存在<" SAVE_PATH ">"))
  (foreach key keyList
    (setq (eval (read (strcat key "_value"))) nil)
  )
)




;命令程序
(defun c:ls-count-curtain-footage( / *error* e_lst)
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
	;程序开??
	(vl-load-com)
	(setq teston nil)
	(if teston
		(test)
		(start_program)
	)
	; (start_program)

	;程序结束
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)