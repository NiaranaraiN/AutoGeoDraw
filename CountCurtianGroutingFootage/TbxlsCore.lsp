;基础库

(defun vls-core-load ( / result)
	(setq result "Load TbxlsCore Success")
	result
)


; (defun getuip( domain / ob ipv4Addr)
;         (setq ob (vlax-create-object "Msxml2.XMLHTTP"))
;         (vlax-invoke-method ob "open" "get" domain "false")
;         (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke-method (list ob "send")))
;                 (princ "\n&micro;±&Ccedil;°&Icirc;&THORN;&Iacute;&oslash;&Acirc;&ccedil;&Aacute;&not;&frac12;&Oacute;")
;                 (setq ipv4Addr (vlax-get-property ob "responseText"))
;         )
;         (vlax-release-object ob)
;         ipv4Addr
; )

;查询本机的公网ipv4地址
(defun vls-Http-Response( domain / getPost resposeResult)
        (setq getPost (vlax-create-object "Msxml2.XMLHTTP"))
        (vlax-invoke-method getPost "open" "get" domain "false")
        (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke-method (list getPost "send")))
                (princ "\n&micro;±&Ccedil;°&Icirc;&THORN;&Iacute;&oslash;&Acirc;&ccedil;&Aacute;&not;&frac12;&Oacute;")
                (setq resposeResult (vlax-get-property getPost "responseText"))
        )
        (vlax-release-object getPost)
        resposeResult
)


;设置程序运行时要修改的系统变量及变量值
(defun vls-Modify-SystemVariant (/ modifiedVarlist)
  "设置程序运行时要修改的系统变量及变量值" 
  "返回系统变量名称-值的一个列表" 
  (setq modifiedVarlist
    (list 
      (list "DIMZIN" 0);保留前导和后续的零
      (list "CMDECHO" 0) ;关闭回显
      (list "CLAYER" (getvar "CLAYER")) ;修改图层
      (list "OSMODE" 0) ;关闭对象捕捉
   )
  )
  modifiedVarlist
)