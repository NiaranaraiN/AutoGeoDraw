;--加载区--
	(if (null vlax-dump-object) (vl-load-com));将Visual LISP扩展功能加载到 AutoLISP
	(load "TbxlsCore")
;--*--



(defun vls-Start-foo ()
(setq ss (ssget '((0 . "CIRCLE")))) ;; 获取当前图中所有圆
  (if ss ;; 判断是否存在选中的圆
    (progn
      ;; 遍历选择集中的每个圆对象
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i)))) ;; 获取圆对象
        (setq obj (vlax-ename->vla-object ent)) ;; 转换为 VLA 对象
        ;; 获取圆的中心点和半径
        (setq center (vla-get-Center obj))
        (setq radius (vla-get-Radius obj))
        
        ;; 创建一个新的 Hatch 填充
        (setq hatch (vla-AddHatch
                      (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                      acHatchPatternTypePreDefined
                      "SOLID" ;; 填充为实心
                      :vlax-true)) ;; 边界关联
        
        ;; 创建填充边界，即一个圆形
        (vla-AppendOuterLoop hatch (list obj)) ;; 使用圆的边界作为填充的边界
        ;; 设置填充颜色，如果需要可以调整
        (vla-put-Color hatch 1) ;; 设置为红色，AutoCAD标准颜色索引 1
        
        ;; 打印调试信息（可选）
        (princ (strcat "\n完成一个圆的填充，中心点: " (vl-princ-to-string center)))
      )
      (princ "\n所有圆已经添加填充覆盖。")
    )
    (princ "\n未找到任何圆对象！")
  )
  (princ)
)


;命令程序
(defun C:plqyfg (/ *error* *CurDoc* *Old_SysVar*) 
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
  (vls-Start-foo)
  ;--程序结尾--
    (command "redraw");清空一行
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)));参数恢复
    (vla-endundomark *CurDoc*) ;结束编组
    (princ)
)

