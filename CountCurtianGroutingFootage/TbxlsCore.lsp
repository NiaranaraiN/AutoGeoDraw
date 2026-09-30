       

(defun vls-core-load ( / __result.any)
	(setq __result.any "Load TbxlsCore Success")
	__result.any
)


                                      
                                                         
                                                             
                                                                                              
                                                                                                                           
                                                                       
           
                                  
                  
   

                       
(defun vls-Http-Response( self.domain.str / __get_post.any __respose_result.any)
        (setq __get_post.any (vlax-create-object "Msxml2.XMLHTTP"))
        (vlax-invoke-method __get_post.any "open" "get" self.domain.str "false")
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke-method (list __get_post.any "send")))
                (princ "\n&micro;±&Ccedil;°&Icirc;&THORN;&Iacute;&oslash;&Acirc;&ccedil;&Aacute;&not;&frac12;&Oacute;")
                (setq __respose_result.any (vlax-get-property __get_post.any "responseText"))
        )
        (vlax-release-object __get_post.any)
        __respose_result.any
)


                                       
(defun vls-Modify-SystemVariant (/ __modified_varlist.list)
  "设置程序运行时要修改的系统变量及变量值" 
  "返回系统变量名称-值的一个列表" 
  (setq __modified_varlist.list
    (list 
      (list "DIMZIN" 0)                   
      (list "CMDECHO" 0)          
      (list "CLAYER" (getvar "CLAYER"))          
      (list "OSMODE" 0)              
   )
  )
  __modified_varlist.list
)