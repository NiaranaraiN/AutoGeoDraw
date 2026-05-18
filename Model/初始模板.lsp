;--加载区--
	(if (null vlax-dump-object) (vl-load-com));将Visual LISP扩展功能加载到 AutoLISP
	(if (null vls-core-load) (load "TbxlsCore"))
;--*--

(defun test ( / )
	
)

(defun start-program ( / )
  (test)
)

;命令程序
(defun C:foo (/ *error* *CurDoc* *Old_SysVar*) 
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
  (start-program)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)
