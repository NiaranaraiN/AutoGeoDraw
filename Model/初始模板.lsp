           
	(if (null vlax-dump-object) (vl-load-com))                                     
	(if (null vls-core-load) (load "TbxlsCore"))
      

(defun test ( / )
	
)

(defun start_program ( / )
  (test)
)

         
(defun C:foo (/ *error* __cur_doc.obj __old_sys_var.any __recoversystemvar.any __xx.any) 
                 
    (setq __cur_doc.obj (vla-get-activedocument (vlax-get-acad-object)))
    (defun *error* (self.msg.str)           
                         
      (if __old_sys_var.any (foreach __recoversystemvar.any __old_sys_var.any (apply 'setvar __recoversystemvar.any)))         
      (vla-endundomark __cur_doc.obj)                
    )

    (while (eq 8 (logand 8 (getvar 'undoctl))) 
      (vla-endundomark __cur_doc.obj)
    )                
    
    (vla-startundomark __cur_doc.obj)          
      (progn              
        (setq __old_sys_var.any '())                  
        (setq __old_sys_var.any 
            (mapcar 
              '(lambda (self.modified_system_var.any / __init_system_value.any) 
                (if (setq __init_system_value.any (getvar (car self.modified_system_var.any)))                                              
                    (progn 
                      (vl-catch-all-apply 'setvar self.modified_system_var.any)                               
                      (list (car self.modified_system_var.any) __init_system_value.any)                                               
                    )
                )
              )
                                                 
              (vls-Modify-SystemVariant)                                              
            )
        )         
      )
  (start_program)
               
    (command "redraw")         
    (if __old_sys_var.any (foreach __xx.any __old_sys_var.any (apply 'setvar __xx.any)))         
    (vla-endundomark __cur_doc.obj)          
    (princ)
)
