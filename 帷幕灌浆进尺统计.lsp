                                 
	(if (null vlax-dump-object) (vl-load-com))                                     
	(load "TbxlsCore")
                                 
                        
(defun bakup_env()
	(setq osmode_ctrl (getvar "osmode"))              
	(setq clayer_ctrl (getvar "clayer"))              
)         

                 
(defun recover_env()
  	(setvar "osmode" osmode_ctrl)
  	(setvar "clayer" clayer_ctrl)	
)

                
(defun get_coorx( pline / start_point end_point pline_coorx)
		                          
			(setq start_point (vlax-curve-getstartpoint pline))
			(setq start_point (car start_point))
			(setq end_point (vlax-curve-getendpoint pline))
			(setq end_point (car end_point))
		                   
			(if (< start_point end_point)
				(setq pline_coorx (list start_point end_point))
				(setq pline_coorx (list end_point start_point))
      )
			pline_coorx
)


                                           
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
)               

                              
(defun cuntain_start_point_coordinate (curtain_line_set get_method / coordinate_list item_coordinate item_index set_length result )
	"提取线段选择集内的起点坐标值,
	如果'get_method'为'X'则提取X坐标值，
	如果'get_method'为'Y'则提取Y坐标值，
	" 
	"@returns 返回一个列表，包含所有X或Y坐标值"
	    
		(setq set_length (sslength curtain_line_set))
		(setq  item_index 0)             
		(setq coordinate_list '())               
		(while (< item_index set_length)
			(setq item (ssname curtain_line_set item_index))                   
			(cond
				((= get_method "X_VALUE") (setq item_coordinate (car (vlax-curve-getstartpoint item))))                   
				((= get_method "Y_VALUE") (setq item_coordinate (cadr (vlax-curve-getstartpoint item))))                   
			)                 
			(setq coordinate_list (append coordinate_list (list item_coordinate)))
			(setq item_index (1+ item_index))
		)
		(setq result coordinate_list)
		result
		                                                                          
)

                                                                                                                                                                      
(defun caculate_real_data (raw_data consult_data consult_actual_value actual_scale / result )  "输入需要计算点的Y值、参照点的Y值、参照点实际坐标值及图纸比例，计算出计算点的高程值" "真实高程值" 
	(setq result (+ consult_actual_value (* (- raw_data consult_data) (/ actual_scale 1000) ) ) )
  (setq result (read (rtos result 2 *result_precison*)))
  result
)
                                   
                                                                                                        
(defun get_inter_point (pline1 pline2 / obj_inter_result result obj_1 obj_2) "求两个布线的交点。并返回交点坐标值（本程序中帷幕灌浆与各界限交点仅有0~1个值，所以不考虑多个交点的问题。" "返回交点坐标，列表型"
  (setq obj_1 (vlax-ename->vla-object pline1))
  (setq obj_2 (vlax-ename->vla-object pline2))
  (setq obj_inter_result (vlax-variant-value (vlax-invoke-method obj_1 'IntersectWith obj_2 0)))
  (setq result (vlax-safearray->list obj_inter_result))
  result
)
 

                                                                 
(defun stratum_inter (curtain_bore curtain_bore_coorx pline_set pline_set_coorx_list / inter_point stratum_line pline_coorx_min pline_coorx_max set_length set_index result) "求一条多段线与一组多段线是否存在交点，如果存在，取得该点的坐标值" "点坐标值"
	(setq set_length (sslength pline_set))
	(setq set_index 0 result '(0 0 0))
	(while (< set_index set_length)
    (setq pline_coorx_min (car (nth set_index pline_set_coorx_list)))              
    (setq pline_coorx_max (cadr (nth  set_index pline_set_coorx_list)))              
    (if (and (>= curtain_bore_coorx pline_coorx_min) (< curtain_bore_coorx pline_coorx_max))                               
               
        (progn 
					(setq stratum_line (ssname pline_set set_index))                                               
					(setq inter_point (get_inter_point curtain_bore stratum_line ))           
					(setq result inter_point)
					(setq set_index (1+ set_length))         
        )
             
        (setq set_index (1+ set_index))
    )
  )
  result
)
        
                                 
(defun stratum_length (curtain_bore curtain_bore_coorx delta_elevation pline_set pline_set_coorx_list bore_top_elevation water_actual_elevation draw_scale / arg1 arg2 arg3 arg4 stratum_inter_point stratum_inter_coorx stratum_inter_coory stratum_elevation result)
  (setq arg1 curtain_bore arg2 curtain_bore_coorx arg3 pline_set arg4 pline_set_coorx_list)
  (if arg3                 
  (progn
                                                                                                                            
    (setq stratum_inter_point (stratum_inter arg1 arg2 arg3 arg4))             
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

                                               
(defun remove_overlap_length ( arg1 arg2 arg3 / length_list list_index_list value_max value_mid value_min remove_overlap_list i item_value item result)
  "去除土体、坝体、砼进尺同时存在时上部重复的部分，strtum_length计算的是从孔口至层底限,所以如果同时存在，上部分有重复的。思路则是三者对比排序，最大-次大，次大-最小，得到最新的数据" 
  ""
  (setq length_list (list arg1 arg2 arg3))
  (setq list_index_list (vl-sort-i length_list '>))                                
  (setq value_max (nth (nth 0  list_index_list) length_list ))
  (setq value_mid (nth (nth 1 list_index_list) length_list))
  (setq value_min (nth (nth 2 list_index_list) length_list))
  (setq value_max (- value_max value_mid))           
  (setq value_mid (- value_mid value_min))           
  (setq remove_overlap_list (list value_max value_mid value_min))                                  
  (setq i 0 result '(99990 99991 99992))                                   
  (foreach item list_index_list
    (setq item_value (nth i remove_overlap_list))
    (setq result (subst item_value (+ 99990 item) result))
    (setq i (1+ i))
	)
  result
)
	


                 
(defun print_value_prop (get_value / ) "函数功能说明" "函数返回值"
      (princ "\n类型：")
      (princ (type get_value))
      (princ "\n数值：")
      (princ (rtos get_value))
      (princ "\n ------------")
      (princ "\n")
  )

                                                                    

(defun test( / result)
  (setq result (remove_overlap_length 12 10 13))
  (princ "\n------")
  (princ result)
  (princ "\n------")
)


          
(defun start_program ()
  (defun action-format-keys (key)                        
    (cond
      ((= key "accept")            
        (key-value-bak)
        (done_dialog 1)                                              
      )
      ((= key "cancel")            
        (done_dialog 0)                                              
      )
      ((= key "SSCurtainButtom")             
        (key-value-bak)
        (done_dialog 3)
      )
      ((= key "SelectCurtainStatus")                 
        ()
      )
      ((= key "SSWaterLevelButtom")                   
        (key-value-bak) 
        (done_dialog 4)
      )
      ((= key "drawingScaleInput")           
        ()
      )
      ((= key "WaterLevelElevationInput")                       
        ()
      )
      ((= key "SSGroundButtom")             
        (key-value-bak)
        (done_dialog 5)
      )
      ((= key "SelectGroundStatus")                 
        ()
      )
      ((= key "SSSoilButtom")             
        (key-value-bak)
        (done_dialog 6)
      )
      ((= key "SelectSoilStatus")                 
        ()
      )
      ((= key "SSDamTopButtom")             
        (key-value-bak)
        (done_dialog 7)
      )
      ((= key "SelectDamTopStatus")                 
        ()
      )
      ((= key "SSDamBottomButtom")             
        (key-value-bak)
        (done_dialog 8)
      )
      ((= key "SelectDamBottomStatus")                 
        ()
      )
      ((= key "SSConcreteButtom")                               
        (key-value-bak)
        (done_dialog 9)
      )
      ((= key "SelectConcreteStatus")                     
        ()
      )
      ((= key "SelectResetButtom")                 
        (key-value-bak)
        (done_dialog 10)
      )
      ((= key "SavePathButtom")               
        (key-value-bak)
        (done_dialog 11)
      )
      ((= key "ShowSavePath")               
        ()
      )
    )
  ) 

  (defun key-value-bak ( / key )                  
    (foreach key keyList
      (set (read (strcat key "_value")) (get_tile key))
    )
    
  )

  (defun botton-action (bottonId / ss_length)
    (cond 
      ( (= bottonId 3)                 
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


      ((= bottonId 10)      
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
                                       


      (foreach key keyList
        (if (eval (read (strcat key "_value")))
          (set_tile key (eval (read (strcat key "_value"))))
        )         
        (action_tile key "(action-format-keys $key)")         
      )



      (setq dialogStatus (start_dialog))

      (botton-action dialogstatus)
             
                            
                         
                                  
                                   
                                              
                             
                                                
                                                    
           
          
         
    )
    (unload_dialog dialogId)
  )

	       
    (load-menu)
    (get-input-value)
                                                    
    (setq *result_precison* 3)
  
                             
		                                                            
		                                
		                          
                    

		                     
		                                                           
		                                         
                                                                
                             
		

		                 
		                          
		                           

		             
		                      
		                          

		                    
 		                                                           
  	                                                                                          
		                                                              
		                                              
		          
		                           
		                              
		                         
		                                 
		     
                 
                                 
                                    
           
       
		
		                       
		                            
		                                               
		(setq delta_elevation (cuntain_start_point_coordinate ss_water_level "Y_VALUE"))
		(setq delta_elevation (car delta_elevation))                                                         

		                    
  	                                                        
		                                                                                              
		                                                         
       
		                                                                   
		          
		                           
		                           
		     
                            
       


		                      
		                                                                  
		                                                                                                             
		                                                                   
       
		                                                        
		          
		                         
		                               
		     
                                
       
	
	                         
	                                                               
	(setq csv_file (open SAVE_PATH "w"))
	(write-line "钻孔编号,孔口高程,孔底高程,孔深,土层进尺,坝体进尺,砼进尺,基岩进尺,有效进尺,无效进尺" csv_file) 
  
                               
	(setq ground_coorx_list (get_coorx_list ss_ground))              
	(setq soil_coorx_list (get_coorx_list ss_soil))              
	                                                                   
	(setq dam_coorx_list (get_coorx_list ss_dam_bottom))            
	(setq concrete_coorx_list (get_coorx_list ss_concrete))               

                                                                                            
	(setq curtain_coorx_list (cuntain_start_point_coordinate ss_curtain "X_VALUE"))                        
	(setq curtain_coory_list (cuntain_start_point_coordinate ss_curtain "Y_VALUE"))                        
	(setq curtain_sorted_index (vl-sort-i curtain_coorx_list '<))                                                                                           

  
                     
	(setq bore_number 1)               
	(foreach curtain_index curtain_sorted_index
		             
			(setq curtain_bore (ssname ss_curtain curtain_index))                   
			(setq curtain_top_coorx (nth curtain_index curtain_coorx_list))          
			(setq curtain_top_coory (nth curtain_index curtain_coory_list))          
			(setq curtain_bottom_coory (nth 1 (vlax-curve-getEndPoint curtain_bore)))          
      (if (< curtain_top_coory curtain_bottom_coory)
          (progn 
            (setq tmp_value curtain_top_coory)   
            (setq curtain_top_coory curtain_bottom_coory)   
            (setq curtain_bottom_coory tmp_value)   
          )
      )
		             
			(setq bore_top_elevation (caculate_real_data curtain_top_coory delta_elevation water_elevation draw_scale))                               
  		(setq bore_bottom_elevation (caculate_real_data curtain_bottom_coory delta_elevation water_elevation draw_scale))                                   
      (setq bore_length (read (rtos (- bore_top_elevation bore_bottom_elevation) 2 *result_precison*)))                    
			(setq soil_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_soil soil_coorx_list bore_top_elevation water_elevation draw_scale))           
			(setq dam_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_dam_bottom dam_coorx_list bore_top_elevation water_elevation draw_scale))            
			(setq concrete_length (stratum_length curtain_bore curtain_top_coorx delta_elevation ss_concrete concrete_coorx_list bore_top_elevation water_elevation draw_scale))         
			                                         
			(setq list_remoe_overlap (remove_overlap_length soil_length dam_length concrete_length))
			(setq soil_length (read (rtos (nth 0 list_remoe_overlap) 2 *result_precison*)))                 
			(setq dam_length (read (rtos (nth 1 list_remoe_overlap) 2 *result_precison*)))                  
			(setq concrete_length (read (rtos (nth 2 list_remoe_overlap) 2 *result_precison*)))               
   
			(setq bedrock_length (read (rtos (- bore_length soil_length dam_length concrete_length) 2 *result_precison*)))           
			(setq invalid_length (- bore_top_elevation water_elevation))         
			(if (< invalid_length 0) (setq invalid_length 0))
      (setq invalid_length (read (rtos invalid_length 2 *result_precison*)))
			(setq effective_length (read (rtos (- bore_length invalid_length dam_length concrete_length) 2 *result_precison*)))         
			                                                                                          
			(setq strcat_to_line 
        (strcat (rtos bore_number 2 *result_precison*) "," (rtos bore_top_elevation 2 *result_precison*) "," (rtos bore_bottom_elevation 2 *result_precison*) "," (rtos bore_length 2 *result_precison*) "," (rtos soil_length 2 *result_precison*) "," (rtos dam_length 2 *result_precison*) "," (rtos concrete_length 2 *result_precison*) "," (rtos bedrock_length 2 *result_precison*) "," (rtos effective_length 2 *result_precison*) "," (rtos invalid_length 2 *result_precison*)
        )
      )
			(write-line strcat_to_line csv_file)
			(setq bore_number (1+ bore_number))
  	)
        
	(close csv_file)
  (alert (strcat "进尺统计完成！已保存在<" SAVE_PATH ">"))
  (foreach key keyList
    (setq (eval (read (strcat key "_value"))) nil)
  )
)




         
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
	         
	(vl-load-com)
	(setq teston nil)
	(if teston
		(test)
		(start_program)
	)
	                 

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)