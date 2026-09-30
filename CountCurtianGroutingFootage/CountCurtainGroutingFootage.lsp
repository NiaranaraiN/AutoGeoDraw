                                 
	(if (null vlax-dump-object) (vl-load-com))                                     
	(load "TbxlsCore")
                                 
                        
(defun bakup_env()
	(setq *osmode_ctrl* (getvar "osmode"))              
	(setq *clayer_ctrl* (getvar "clayer"))              
)         

                 
(defun recover_env()
  	(setvar "osmode" *osmode_ctrl*)
  	(setvar "clayer" *clayer_ctrl*)	
)

                
(defun get_coorx( self.pline.ename / __start_point.pt __end_point.pt __pline_coorx.list)
		                          
			(setq __start_point.pt (vlax-curve-getstartpoint self.pline.ename))
			(setq __start_point.pt (car __start_point.pt))
			(setq __end_point.pt (vlax-curve-getendpoint self.pline.ename))
			(setq __end_point.pt (car __end_point.pt))
		                   
			(if (< __start_point.pt __end_point.pt)
				(setq __pline_coorx.list (list __start_point.pt __end_point.pt))
				(setq __pline_coorx.list (list __end_point.pt __start_point.pt))
      )
			__pline_coorx.list
)


                                           
(defun get_coorx_list(self.pline_set.ss / __pline_index.int __set_length.int __result.any __pline_coorx_result.list) "获取给定曲线的x坐标，并组成集合pline_coorx" "多线起点和终点坐标的列表"
	(setq __pline_index.int 0 __result.any '())
  (if self.pline_set.ss
		(progn
			(setq __set_length.int (sslength self.pline_set.ss))
			(while (< __pline_index.int __set_length.int)
				(setq __pline_coorx_result.list (get_coorx (ssname self.pline_set.ss __pline_index.int)))
				(setq __result.any (append __result.any (list __pline_coorx_result.list)))
				(setq __pline_index.int (+ 1 __pline_index.int))
			)
    )
			(setq __result.any '((0 0)))
  )
	__result.any
)               

                              
(defun cuntain_start_point_coordinate (self.curtain_line_set.ss self.get_method.str / __coordinate_list.list __item_coordinate.pt __item_index.int __set_length.int __result.any  __item.any)
	"提取线段选择集内的起点坐标值,
	如果'get_method'为'X'则提取X坐标值，
	如果'get_method'为'Y'则提取Y坐标值，
	" 
	"@returns 返回一个列表，包含所有X或Y坐标值"
	    
		(setq __set_length.int (sslength self.curtain_line_set.ss))
		(setq  __item_index.int 0)             
		(setq __coordinate_list.list '())               
		(while (< __item_index.int __set_length.int)
			(setq __item.any (ssname self.curtain_line_set.ss __item_index.int))                   
			(cond
				((= self.get_method.str "X_VALUE") (setq __item_coordinate.pt (car (vlax-curve-getstartpoint __item.any))))                   
				((= self.get_method.str "Y_VALUE") (setq __item_coordinate.pt (cadr (vlax-curve-getstartpoint __item.any))))                   
			)                 
			(setq __coordinate_list.list (append __coordinate_list.list (list __item_coordinate.pt)))
			(setq __item_index.int (1+ __item_index.int))
		)
		(setq __result.any __coordinate_list.list)
		__result.any
		                                                                          
)

                                                                                                                                                                      
(defun caculate_real_data (self.raw_data.str self.consult_data.any self.consult_actual_value.any self.actual_scale.real / __result.any )  "输入需要计算点的Y值、参照点的Y值、参照点实际坐标值及图纸比例，计算出计算点的高程值" "真实高程值" 
	(setq __result.any (+ self.consult_actual_value.any (* (- self.raw_data.str self.consult_data.any) (/ self.actual_scale.real 1000) ) ) )
  (setq __result.any (read (rtos __result.any 2 *result_precison*)))
  __result.any
)


                                                                                                        
(defun get_inter_point (self.pline1.ename self.pline2.ename / __obj_inter_result.obj __result.any __obj_1.obj __obj_2.obj) "求两个布线的交点。并返回交点坐标值（本程序中帷幕灌浆与各界限交点仅有0~1个值，所以不考虑多个交点的问题。" "返回交点坐标，列表型"
  (setq __obj_1.obj (vlax-ename->vla-object self.pline1.ename))
  (setq __obj_2.obj (vlax-ename->vla-object self.pline2.ename))
  (setq __obj_inter_result.obj (vlax-variant-value (vlax-invoke-method __obj_1.obj 'IntersectWith __obj_2.obj 0)))
  (setq __result.any (vlax-safearray->list __obj_inter_result.obj))
  __result.any
)

                                                                 
(defun stratum_inter (self.curtain_bore.any self.curtain_bore_coorx.pt self.pline_set.ss self.pline_set_coorx_list.list / __inter_point.pt __stratum_line.str __pline_coorx_min.pt __pline_coorx_max.pt __set_length.int __set_index.int __result.any) "求一条多段线与一组多段线是否存在交点，如果存在，取得该点的坐标值" "点坐标值"
	(setq __set_length.int (sslength self.pline_set.ss))
	(setq __set_index.int 0 __result.any '(0 0 0))
	(while (< __set_index.int __set_length.int)
    (setq __pline_coorx_min.pt (car (nth __set_index.int self.pline_set_coorx_list.list)))              
    (setq __pline_coorx_max.pt (cadr (nth  __set_index.int self.pline_set_coorx_list.list)))              
    (if (and (>= self.curtain_bore_coorx.pt __pline_coorx_min.pt) (< self.curtain_bore_coorx.pt __pline_coorx_max.pt))                               
               
        (progn 
					(setq __stratum_line.str (ssname self.pline_set.ss __set_index.int))                                               
					(setq __inter_point.pt (get_inter_point self.curtain_bore.any __stratum_line.str ))           
					(setq __result.any __inter_point.pt)
					(setq __set_index.int (1+ __set_length.int))         
        )
             
        (setq __set_index.int (1+ __set_index.int))
    )
  )
  __result.any
)


        
                                 
(defun stratum_length (self.curtain_bore.any self.curtain_bore_coorx.pt self.delta_elevation.any self.pline_set.ss self.pline_set_coorx_list.list self.bore_top_elevation.any self.water_actual_elevation.any self.draw_scale.real / __arg1.any __arg2.any __arg3.any __arg4.any __stratum_inter_point.pt __stratum_inter_coorx.pt __stratum_inter_coory.pt __stratum_elevation.str __result.any)
  (setq __arg1.any self.curtain_bore.any __arg2.any self.curtain_bore_coorx.pt __arg3.any self.pline_set.ss __arg4.any self.pline_set_coorx_list.list)
  (if __arg3.any                 
  (progn
                                                                                                                            
    (setq __stratum_inter_point.pt (stratum_inter __arg1.any __arg2.any __arg3.any __arg4.any))             
    (setq __stratum_inter_coorx.pt (nth 0 __stratum_inter_point.pt))
    (setq __stratum_inter_coory.pt (nth 1 __stratum_inter_point.pt))
    (if (and (= __stratum_inter_coorx.pt 0) (= __stratum_inter_coory.pt 0))
      (setq __result.any 0)
      (progn
        (setq __stratum_elevation.str (caculate_real_data __stratum_inter_coory.pt self.delta_elevation.any self.water_actual_elevation.any self.draw_scale.real))
        (setq __result.any (- self.bore_top_elevation.any __stratum_elevation.str))
      )
    )
    
    (setq __result.any (read (rtos __result.any 2 *result_precison*)))
  )
    (setq __result.any 0)
  )
  __result.any
)

                                               
(defun remove_overlap_length ( self.arg1.any self.arg2.any self.arg3.any / __length_list.list __list_index_list.int __value_max.any __value_mid.any __value_min.any __remove_overlap_list.list __i.int __item_value.any __item.any __result.any)
  "去除土体、坝体、砼进尺同时存在时上部重复的部分，strtum_length计算的是从孔口至层底限,所以如果同时存在，上部分有重复的。思路则是三者对比排序，最大-次大，次大-最小，得到最新的数据" 
  ""
  (setq __length_list.list (list self.arg1.any self.arg2.any self.arg3.any))
  (setq __list_index_list.int (vl-sort-i __length_list.list '>))                                
  (setq __value_max.any (nth (nth 0  __list_index_list.int) __length_list.list ))
  (setq __value_mid.any (nth (nth 1 __list_index_list.int) __length_list.list))
  (setq __value_min.any (nth (nth 2 __list_index_list.int) __length_list.list))
  (setq __value_max.any (- __value_max.any __value_mid.any))           
  (setq __value_mid.any (- __value_mid.any __value_min.any))           
  (setq __remove_overlap_list.list (list __value_max.any __value_mid.any __value_min.any))                                  
  (setq __i.int 0 __result.any '(99990 99991 99992))                                   
  (foreach __item.any __list_index_list.int
    (setq __item_value.any (nth __i.int __remove_overlap_list.list))
    (setq __result.any (subst __item_value.any (+ 99990 __item.any) __result.any))
    (setq __i.int (1+ __i.int))
	)
  __result.any
)
	


                 
(defun print_value_prop (self.get_value.any / ) "函数功能说明" "函数返回值"
      (princ "\n类型：")
      (princ (type self.get_value.any))
      (princ "\n数值：")
      (princ (rtos self.get_value.any))
      (princ "\n ------------")
      (princ "\n")
  )

                                                                    

(defun test( / __result.any)
  (setq __result.any (remove_overlap_length 12 10 13))
  (princ "\n------")
  (princ __result.any)
  (princ "\n------")
)


          
(defun start_program ( /  __ss_curtain.any __ss_water_level.any __ss_ground.any __ss_soil.any __ss_dam_top.any __ss_dam_bottom.any __ss_concrete.any __dialogstatus.bool __save_path.str __water_elevation.any __dialogfile.str __dialogname.str __dialogid.any __keylist.list __csv_file.file __ground_coorx_list.list __soil_coorx_list.list __dam_coorx_list.list __concrete_coorx_list.list __curtain_coorx_list.list __curtain_coory_list.list __curtain_sorted_index.int __bore_number.int __curtain_index.int __curtain_top_coorx.pt __curtain_top_coory.pt __curtain_bottom_coory.pt __bore_bottom_elevation.any __bore_length.int __soil_length.int __dam_length.int __concrete_length.int __list_remoe_overlap.list __bedrock_length.int __invalid_length.int __effective_length.int __strcat_to_line.str __key.str)
  (defun action_format_keys (self.key.str)                        
    (cond
			((= self.key.str "help")            
        (key_value_bak)
        (done_dialog 2)                                              
      )
      ((= self.key.str "accept")            
        (key_value_bak)
        (done_dialog 1)                                              
      )
      ((= self.key.str "cancel")            
        (done_dialog 0)                                              
      )
      ((= self.key.str "SSCurtainButtom")             
        (key_value_bak)
        (done_dialog 3)
      )
      ((= self.key.str "SelectCurtainStatus")                 
        ()
      )
      ((= self.key.str "SSWaterLevelButtom")                   
        (key_value_bak) 
        (done_dialog 4)
      )
      ((= self.key.str "drawingScaleInput")           
        ()
      )
      ((= self.key.str "WaterLevelElevationInput")                       
        ()
      )
      ((= self.key.str "SSGroundButtom")             
        (key_value_bak)
        (done_dialog 5)
      )
      ((= self.key.str "SelectGroundStatus")                 
        ()
      )
      ((= self.key.str "SSSoilButtom")             
        (key_value_bak)
        (done_dialog 6)
      )
      ((= self.key.str "SelectSoilStatus")                 
        ()
      )
      ((= self.key.str "SSDamTopButtom")             
        (key_value_bak)
        (done_dialog 7)
      )
      ((= self.key.str "SelectDamTopStatus")                 
        ()
      )
      ((= self.key.str "SSDamBottomButtom")             
        (key_value_bak)
        (done_dialog 8)
      )
      ((= self.key.str "SelectDamBottomStatus")                 
        ()
      )
      ((= self.key.str "SSConcreteButtom")                               
        (key_value_bak)
        (done_dialog 9)
      )
      ((= self.key.str "SelectConcreteStatus")                     
        ()
      )
      ((= self.key.str "SelectResetButtom")                 
        (key_value_bak)
        (done_dialog 10)
      )
      ((= self.key.str "SavePathButtom")               
        (key_value_bak)
        (done_dialog 11)
      )
      ((= self.key.str "ShowSavePath")               
        ()
      )
    )
  ) 

  (defun key_value_bak ( / __key.str )                  
    (foreach __key.str __keylist.list
      (set (read (strcat "*" __key.str "_value*")) (get_tile __key.str))
    )
    
  )

  (defun botton_action (self.botton_id.int / __ss_length.int)
    (cond 
			( (= self.botton_id.int 2)                 
				(alert "注意：\n    使用时请确保图纸单位为“米”，模型空间比例为1：1,以免程序错误。\n    程序想要得到正确结果，必选线段（灌浆线、正常蓄水位线）、剖面图比例、正常蓄水位高程、保存路径必须填写，否则程序会出错。\n     程序为初始开发版，请结果可能会有一些不准确性，数据统计后请自行比对结果的可靠性！\n    程序运行过程中如有问题可联系 mail@liusha.li\n    \n    \n    \n作者:Liusha.Li(email:mail@liusha.li)   2024.05"
        )
      )
      ( (= self.botton_id.int 3)                 
        (princ "\n 选择帷幕灌浆线:")
        (setq __ss_curtain.any (ssget))
        (setq __ss_length.int (sslength __ss_curtain.any))
        (if (= __ss_length.int 0)
          (setq *selectcurtainstatus_value* "无选择")
          (setq *selectcurtainstatus_value* (strcat "已选" (itoa __ss_length.int)))
        )
      )

      ( (= self.botton_id.int 4) 
        (princ "\n 选择正常蓄水位线:")
        (setq __ss_water_level.any (ssget))
        (setq __ss_length.int (sslength __ss_water_level.any))
        (setq *selectwaterlevelbotton_value* __ss_water_level.any)
        (if (= __ss_length.int 0)
          (setq *selectwaterlevelstatus_value* "无选择")
					(setq *selectwaterlevelstatus_value* (strcat "已选" (itoa __ss_length.int)))
      	)
			)

			( (= self.botton_id.int 5)
				(princ "\n 选择地表面线:")
				(setq __ss_ground.any (ssget))
				(setq __ss_length.int (sslength __ss_ground.any))
				(if (= __ss_length.int 0)
					(setq *selectgroundstatus_value* "无选择")
					(setq *selectgroundstatus_value* (strcat "已选" (itoa __ss_length.int)))	
				)      
			)


      ((= self.botton_id.int 6)
        (princ "\n 选择覆盖层与基岩分界线:")
        (setq __ss_soil.any (ssget))
        (setq __ss_length.int (sslength __ss_soil.any))
        (if (= __ss_length.int 0)
          (setq *selectsoilstatus_value* "无选择")
          (setq *selectsoilstatus_value* (strcat "已选" (itoa __ss_length.int)))
        )     
      )


      ((= self.botton_id.int 7)
        (princ "\n 选择坝顶线:")
        (setq __ss_dam_top.any (ssget))
        (setq __ss_length.int (sslength __ss_dam_top.any))
        (if (= __ss_length.int 0)
          (setq *selectdamtopstatus_value* "无选择")
          (setq *selectdamtopstatus_value* (strcat "已选" (itoa __ss_length.int)))
        )        
      )


      ((= self.botton_id.int 8)
        (princ "\n 选择坝底线:")
        (setq __ss_dam_bottom.any (ssget))
        (setq __ss_length.int (sslength __ss_dam_bottom.any))
        (if (= __ss_length.int 0)
          (setq *selectdambottomstatus_value* "无选择")
          (setq *selectdambottomstatus_value* (strcat "已选" (itoa __ss_length.int)))
        )            
      )


      ((= self.botton_id.int 9)
        (princ "\n 选择其它分界线:")
        (setq __ss_concrete.any (ssget))
        (setq __ss_length.int (sslength __ss_concrete.any))
        (if (= __ss_length.int 0)
          (setq *selectconcretestatus_value* "无选择")
          (setq *selectconcretestatus_value* (strcat "已选" (itoa __ss_length.int)))
        )                   
      )


      ((= self.botton_id.int 10)      
        (setq *selectcurtainstatus_value* "无选择")
        (setq *selectwaterlevelstatus_value* "无选择")
        (setq *sselectgroundstatus_value* "无选择")
        (setq *selectsoilstatus_value* "无选择")
        (setq *selectdamtopstatus_value* "无选择")
        (setq *selectdambottomstatus_value* "无选择")
        (setq *selectconcretestatus_value* "无选择")
        (setq *waterlevelelevationinput_value* "")
        (setq *drawingscaleinput_value* "")
        (setq *showsavepath_value* "请选择保存路径！")
        (setq __dialogstatus.bool 2 __ss_curtain.any nil __ss_ground.any nil __ss_dam_top.any nil __ss_dam_bottom.any nil __ss_water_level.any nil __ss_soil.any nil __ss_concrete.any nil ) 
      )


      ((= self.botton_id.int 11)
        (setq __save_path.str (getfiled "选择数据保存位置" *showsavepath_value* "csv" 1))
        (setq *showsavepath_value* __save_path.str)
      )
    )
    

  )

  (defun get_input_value ()
    (setq *draw_scale*  (read *drawingscaleinput_value*))
    (setq __water_elevation.any  (read *waterlevelelevationinput_value*))
    
  )

  (defun load_menu ()
    (setq __dialogfile.str "W:\\Plugin\\LISP\\AutoGeoDraw\\CountCurtianGroutingFootage\\CountCurtainGroutingFootageDCL.dcl")
                                                            
    (setq __dialogname.str "MainMenu")
    (if (= __dialogname.str "") (exit))
    (setq __dialogid.any (load_dialog __dialogfile.str))
    (if (< __dialogid.any 0)(EXIT))
    (setq __dialogstatus.bool 2)
    (setq __keylist.list '("SSCurtainButtom" "SelectCurtainStatus" "SSWaterLevelButtom" "SelectWaterLevelStatus" "drawingScaleInput" "WaterLevelElevationInput" "SSGroundButtom" "SelectGroundStatus" "SSSoilButtom" "SelectSoilStatus" "SSDamTopButtom" "SelectDamTopStatus" "SSDamBottomButtom" "SelectDamBottomStatus" "SSConcreteButtom" "SelectConcreteStatus" "SelectResetButtom" "SavePathButtom" "ShowSavePath" "accept" "cancel" "help"))
    (while (> __dialogstatus.bool 1)
      (if (not (new_dialog __dialogname.str __dialogid.any)) (exit))
      (foreach __key.str __keylist.list
        (if (eval (read (strcat "*" __key.str "_value*")))
          (set_tile __key.str (eval (read (strcat "*" __key.str "_value*"))))
        )         
				(action_tile __key.str "(action_format_keys $key)")         
      )
      (setq __dialogstatus.bool (start_dialog))
      (botton_action __dialogstatus.bool)
    )
    (unload_dialog __dialogid.any)
  )

     
                                                                   
	      
	           
	                                                 
	            
	           
         
     
  
	       
		(setq __ss_curtain.any nil __ss_ground.any nil __ss_dam_top.any nil __ss_dam_bottom.any nil __ss_water_level.any nil __ss_soil.any nil __ss_concrete.any nil __save_path.str nil )
		(setq *selectcurtainstatus_value* "无选择")
		(setq *selectwaterlevelstatus_value* "无选择")
		(setq *selectgroundstatus_value* "无选择")
		(setq *selectsoilstatus_value* "无选择")
		(setq *selectdamtopstatus_value* "无选择")
		(setq *selectdambottomstatus_value* "无选择")
		(setq *selectconcretestatus_value* "无选择")
		(setq *showsavepath_value* "请选择保存路径！")
    (load_menu)
    (get_input_value)
                                                    
    (setq *result_precison* 3)
                             
		(while (and (= __dialogstatus.bool 1) (not (numberp *draw_scale*)))
			(alert "请输入图形比例！")
			(setq *draw_scale* (getreal "图形比例:<1000>"))             
		)
		                                
		                          
                    

		                     
		                                                           
		(while (and (= __dialogstatus.bool 1) (not (numberp __water_elevation.any)))
    	(setq __water_elevation.any (getreal "请输入正常蓄水位高程："))
    )                      
		

		                 
		                          
		                           

		             
		                      
		                          

		                    
 		                                                           
  	                                                                                          
		                                                              
		                                              
		          
		                           
		                              
		                         
		                                 
		     
                 
                                 
                                    
           
       
		
		                       
		                            
		                                               
		(setq *delta_elevation* (cuntain_start_point_coordinate __ss_water_level.any "Y_VALUE"))
		(setq *delta_elevation* (car *delta_elevation*))                                                         
		                    
  	                                                        
		                                                                                              
		                                                         
       
		                                                                   
		          
		                           
		                           
		     
                            
       


		                      
		                                                                  
		                                                                                                             
		                                                                   
       
		                                                        
		          
		                         
		                               
		     
                                
       
	
	                         
	                                                               
  
	(while (and (= __dialogstatus.bool 1) (or (not __save_path.str) (= __save_path.str "请选择保存路径！")))
			(progn 
			(alert "选择数据保存位置！")
     	(setq __save_path.str (getfiled "选择数据保存位置" "" "csv" 1))
			)
  
  ) 
	(setq __csv_file.file (open __save_path.str "w"))
	(write-line "钻孔编号,孔口高程,孔底高程,孔深,土层进尺,坝体进尺,砼进尺,基岩进尺,有效进尺,无效进尺" __csv_file.file) 
  
                               
	(setq __ground_coorx_list.list (get_coorx_list __ss_ground.any))              
	(setq __soil_coorx_list.list (get_coorx_list __ss_soil.any))              
	                                                                   
	(setq __dam_coorx_list.list (get_coorx_list __ss_dam_bottom.any))            
	(setq __concrete_coorx_list.list (get_coorx_list __ss_concrete.any))               

                                                                                            
	(setq __curtain_coorx_list.list (cuntain_start_point_coordinate __ss_curtain.any "X_VALUE"))                        
	(setq __curtain_coory_list.list (cuntain_start_point_coordinate __ss_curtain.any "Y_VALUE"))                        
	(setq __curtain_sorted_index.int (vl-sort-i __curtain_coorx_list.list '<))                                                                                           

  
                     
	(setq __bore_number.int 1)               
	(foreach __curtain_index.int __curtain_sorted_index.int
		             
			(setq *curtain_bore* (ssname __ss_curtain.any __curtain_index.int))                   
			(setq __curtain_top_coorx.pt (nth __curtain_index.int __curtain_coorx_list.list))          
			(setq __curtain_top_coory.pt (nth __curtain_index.int __curtain_coory_list.list))          
			(setq __curtain_bottom_coory.pt (nth 1 (vlax-curve-getEndPoint *curtain_bore*)))          
      (if (< __curtain_top_coory.pt __curtain_bottom_coory.pt)
          (progn 
            (setq *tmp_value* __curtain_top_coory.pt)   
            (setq __curtain_top_coory.pt __curtain_bottom_coory.pt)   
            (setq __curtain_bottom_coory.pt *tmp_value*)   
          )
      )
		             
			(setq *bore_top_elevation* (caculate_real_data __curtain_top_coory.pt *delta_elevation* __water_elevation.any *draw_scale*))                               
  		(setq __bore_bottom_elevation.any (caculate_real_data __curtain_bottom_coory.pt *delta_elevation* __water_elevation.any *draw_scale*))                                   
      (setq __bore_length.int (read (rtos (- *bore_top_elevation* __bore_bottom_elevation.any) 2 *result_precison*)))                    
			(setq __soil_length.int (stratum_length *curtain_bore* __curtain_top_coorx.pt *delta_elevation* __ss_soil.any __soil_coorx_list.list *bore_top_elevation* __water_elevation.any *draw_scale*))           
			(setq __dam_length.int (stratum_length *curtain_bore* __curtain_top_coorx.pt *delta_elevation* __ss_dam_bottom.any __dam_coorx_list.list *bore_top_elevation* __water_elevation.any *draw_scale*))            
			(setq __concrete_length.int (stratum_length *curtain_bore* __curtain_top_coorx.pt *delta_elevation* __ss_concrete.any __concrete_coorx_list.list *bore_top_elevation* __water_elevation.any *draw_scale*))         
			                                         
			(setq __list_remoe_overlap.list (remove_overlap_length __soil_length.int __dam_length.int __concrete_length.int))
			(setq __soil_length.int (read (rtos (nth 0 __list_remoe_overlap.list) 2 *result_precison*)))                 
			(setq __dam_length.int (read (rtos (nth 1 __list_remoe_overlap.list) 2 *result_precison*)))                  
			(setq __concrete_length.int (read (rtos (nth 2 __list_remoe_overlap.list) 2 *result_precison*)))               
   
			(setq __bedrock_length.int (read (rtos (- __bore_length.int __soil_length.int __dam_length.int __concrete_length.int) 2 *result_precison*)))           
			(setq __invalid_length.int (- *bore_top_elevation* __water_elevation.any))         
			(if (< __invalid_length.int 0) (setq __invalid_length.int 0))
      (setq __invalid_length.int (read (rtos __invalid_length.int 2 *result_precison*)))
			(setq __effective_length.int (read (rtos (- __bore_length.int __invalid_length.int __dam_length.int __concrete_length.int) 2 *result_precison*)))         
			                                                                                          
			(setq __strcat_to_line.str 
        (strcat (rtos __bore_number.int 2 *result_precison*) "," (rtos *bore_top_elevation* 2 *result_precison*) "," (rtos __bore_bottom_elevation.any 2 *result_precison*) "," (rtos __bore_length.int 2 *result_precison*) "," (rtos __soil_length.int 2 *result_precison*) "," (rtos __dam_length.int 2 *result_precison*) "," (rtos __concrete_length.int 2 *result_precison*) "," (rtos __bedrock_length.int 2 *result_precison*) "," (rtos __effective_length.int 2 *result_precison*) "," (rtos __invalid_length.int 2 *result_precison*)
        )
      )
			(write-line __strcat_to_line.str __csv_file.file)
			(setq __bore_number.int (1+ __bore_number.int))
  	)
        
	(close __csv_file.file)
  (alert (strcat "进尺统计完成！已保存在<" __save_path.str ">"))
                        
                                                    
     
)




         
(defun c:countcurtaingroutingfootage( / *error* __e_lst.any __teston.any)
	(setq __e_lst.any (mapcar (function (lambda (self.n.any) (list 'setvar self.n.any (getvar self.n.any)))) 
	'("autosnap" "osmode" "aperture" "hpspace" "hpassoc" "mirrtext" "auprec" "luprec" "dimzin" "cecolor")))
	(defun *error* (self.msg.str)
		(mapcar 'eval __e_lst.any)
			(if (not (member
									self.msg.str
									'(nil "函数被取消" ";错误:quit / exit abort")
									)
						)
				(princ (strcat ";错误:" self.msg.str))
			)
	)
	(command "undo" "be")
	(bakup_env)
	         
	(vl-load-com)
	(setq __teston.any nil)
	(if __teston.any
		(test)
		(start_program)
	)
	                 

	         
	(recover_env)
	(command "undo" "e")
	(*error* nil)
)