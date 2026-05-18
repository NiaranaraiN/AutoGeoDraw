(defun load-menu-template (dcl_id / set-all-key-value action-tile->function run-function-by-dialog-id load-menu-window dcl_key_list); DCL模版
	(defun set-all-key-value ( / dcl_key ) ;保存已经输入的值
		(foreach dcl_key dcl_key_list
			(set (read (strcat dcl_key "_value")) (get_tile dcl_key))
		)
	)
  
	(defun action-tile->function (recv_key) ;全部按钮的点击动作触发对应值,不同的值 会触发函数botton-action对应程序
		(cond
			((= recv_key "cancel") ;{取消按钮}
				(done_dialog 0)
			)
			((= recv_key "accept") ;{确认按钮}
				(set-all-key-value)
				(done_dialog 1)
			)
			((= recv_key "help") ;{帮助按钮}
				(set-all-key-value)
				(done_dialog 2)
			)
			((= recv_key "其它的KEY值，根据情况改")
				(set-all-key-value)
				(done_dialog 3)
			)
		)
	)
	(defun run-function-by-dialog-id (recv_dialog_id / ss_length) ;按钮对应的具体操作
		(cond
			((= recv_dialog_id 0)
       
			)
		)
	)
	(defun send-global-value () ;特殊情况下，把控件对应的值传递到非局部变量参数；
		;(setq draw_scale  (read drawingScaleInput_value))
	)
	(defun load-menu-window (recv_dcl_id / );加载菜单
		(setq dialog_return_id 2 dialog_name "输入对话框名称")
		(setq dcl_key_list '("输入需要key，组成列表"))
		(while (> dialog_return_id 1) ; dialogStatus 为1时，是按了确定，0是按了取消
			(if (not (new_dialog dialog_name recv_dcl_id)) (exit))
			(foreach key dcl_key_list
				(if (eval (read (strcat key "_value")))
					(set_tile key (eval (read (strcat key "_value"))))
					(set (read (strcat key "_value")) "NULL")
				);控件内容
				(action_tile key "(action-tile->function $key)");点击动作
			)
			(setq dialog_return_id (start_dialog))
			(run-function-by-dialog-id dialog_return_id)
		)
		(unload_dialog recv_dcl_id)
	)
  (load-menu-window dcl_id)
)