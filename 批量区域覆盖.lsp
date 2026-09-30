           
	(if (null vlax-dump-object) (vl-load-com))                                     
	(load "TbxlsCore")
      



(defun vls-Start-foo ()
(setq ss (ssget '((0 . "CIRCLE"))))                      
  (if ss                        
    (progn
                                 
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))              
        (setq obj (vlax-ename->vla-object ent))                   
                               
        (setq center (vla-get-Center obj))
        (setq radius (vla-get-Radius obj))
        
                                  
        (setq hatch (vla-AddHatch
                      (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                      acHatchPatternTypePreDefined
                      "SOLID"              
                      :vlax-true))            
        
                                   
        (vla-AppendOuterLoop hatch (list obj))                              
                                         
        (vla-put-Color hatch 1)                                     
        
                               
        (princ (strcat "\n完成一个圆的填充，中心点: " (vl-princ-to-string center)))
      )
      (princ "\n所有圆已经添加填充覆盖。")
    )
    (princ "\n未找到任何圆对象！")
  )
  (princ)
)


         
(defun C:plqyfg (/ *error* *CurDoc* *Old_SysVar*) 
	               
    (setq *CurDoc* (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (msg)           
                         
      (if *Old_SysVar* (foreach recoverSystemVar *Old_SysVar* (apply 'setvar recoverSystemVar)))         
      (vla-endundomark *CurDoc*)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark *CurDoc*)
    )                
    
    (vla-startundomark *CurDoc*)          
      (progn              
        (setq *Old_SysVar* '())                  
        (setq *Old_SysVar* 
            (mapcar 
              '(lambda (modifiedSystemVar / initSystemValue) 
                (if (setq initSystemValue (getvar (car modifiedSystemVar)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar modifiedSystemVar)                               
                      (list (car modifiedSystemVar) initSystemValue)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (vls-Start-foo)
               
    (command "redraw")         
    (if *Old_SysVar* (foreach xx *Old_SysVar* (apply 'setvar xx)))         
    (vla-endundomark *CurDoc*)          
    (princ)
)

