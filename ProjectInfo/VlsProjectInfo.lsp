; Drawing-level project information and stratigraphy.
(vl-load-com)

(defun project_info_table (self_key_sym / )
  (cdr (assoc self_key_sym (vlax-ldata-get "VLS_PROJECT_INFO" "Tables"))))

(defun project_info_data ( / )
  (project_info_table 'project))

(defun project_info_strata ( / )
  (project_info_table 'strata))

(defun project_info_save (self_data_list self_strata_list / )
  ; Persist both logical tables in one write.
  (vlax-ldata-put "VLS_PROJECT_INFO" "Tables"
    (list (cons 'project self_data_list) (cons 'strata self_strata_list))))

(defun project_info_fail (self_message_str / )
  (setq *project_info_failure* self_message_str)
  (exit))

(defun project_info_gcd_value (self_block_ename / __data_list __attr_ename __attr_list __value_real)
  (setq __data_list (entget self_block_ename))
  (if (= (cdr (assoc 66 __data_list)) 1)
    (progn
      (setq __attr_ename (entnext self_block_ename))
      (while (and __attr_ename (null __value_real)
          (= "ATTRIB" (cdr (assoc 0 (setq __attr_list (entget __attr_ename))))))
        (if (member (strcase (cdr (assoc 2 __attr_list))) '("GCD" "GC" "ELEV" "ELEVATION" "HEIGHT" "Z" "高程"))
          (setq __value_real (distof (vl-string-trim " \t" (cdr (assoc 1 __attr_list))) 2)))
        (setq __attr_ename (entnext __attr_ename)))))
  (if __value_real __value_real
    (caddr (trans (cdr (assoc 10 __data_list)) self_block_ename 0))))

(defun project_info_gcd_stats ( / __ss_set __index_int __value_real __min_real __max_real)
  (setq __ss_set (ssget "_X" '((0 . "INSERT") (8 . "GCD") (410 . "Model")))
    __index_int 0)
  (if __ss_set
    (repeat (sslength __ss_set)
      (setq __value_real (project_info_gcd_value
        (ssname __ss_set (if (> __index_int 32767) (float __index_int) __index_int)))
        __index_int (1+ __index_int))
      (if (and __value_real (> __value_real 0.0))
        (setq __min_real (if __min_real (min __min_real __value_real) __value_real)
          __max_real (if __max_real (max __max_real __value_real) __value_real)))))
  (list (cons 'gcd_min __min_real) (cons 'gcd_max __max_real)))

(defun project_info_gcd_summary (self_data_list / __min_real __max_real)
  (setq __min_real (cdr (assoc 'gcd_min self_data_list))
    __max_real (cdr (assoc 'gcd_max self_data_list)))
  (if __min_real
    (strcat "GCD 最低高程 " (rtos __min_real 2 6) " m，最高高程 " (rtos __max_real 2 6) " m")
    "图纸中没有正值 GCD 高程。"))

(defun project_info_positive_int (self_value_str / __value_int)
  (if (and (wcmatch self_value_str "#*") (not (wcmatch self_value_str "*[~0-9]*"))
      (> (setq __value_int (atoi self_value_str)) 0)
      (= (itoa __value_int) (vl-string-left-trim "0" self_value_str)))
    __value_int))

(defun project_info_strata_less (self_left_list self_right_list / __left_str __right_str __left_int __right_int)
  (setq __left_str (cadr self_left_list) __right_str (cadr self_right_list))
  (cond
    ((/= (car self_left_list) (car self_right_list)) (< (car self_left_list) (car self_right_list)))
    ((= __left_str __right_str) nil)
    ((= __left_str "") T)
    ((= __right_str "") nil)
    (T
      (setq __left_int (project_info_positive_int __left_str)
        __right_int (project_info_positive_int __right_str))
      (cond
        ((and __left_int __right_int (/= __left_int __right_int)) (< __left_int __right_int))
        ((and __left_int (null __right_int)) T)
        ((and __right_int (null __left_int)) nil)
        (T (< __left_str __right_str))))))

(defun project_info_validate_strata (self_rows_list / __row_list __order_int __sub_str __code_str __key_str __position_list __keys_list __positions_list __strata_list __rock_bool __error_str)
  (while (and self_rows_list (null __error_str))
    (setq __row_list (car self_rows_list) self_rows_list (cdr self_rows_list))
    (if (or (/= (car __row_list) "") (/= (cadr __row_list) "")
        (/= (caddr __row_list) "") (/= (nth 4 __row_list) ""))
      (progn
        (setq __order_int (project_info_positive_int (car __row_list))
          __sub_str (cadr __row_list) __code_str (caddr __row_list)
          __key_str (if (= __sub_str "") __code_str (strcat __code_str "/" __sub_str))
          __position_list (list __order_int __sub_str))
        (cond
          ((null __order_int) (setq __error_str "层序编号必须是正整数。"))
          ((= __code_str "") (setq __error_str "请填写地层代号。"))
          ((vl-string-search "/" __sub_str) (setq __error_str "亚层序中不能使用斜线 /。"))
          ((member __position_list __positions_list) (setq __error_str "层序编号与亚层序的组合必须唯一。"))
          ((member __key_str __keys_list) (setq __error_str (strcat "完整地层编号重复：" __key_str)))
          ((or (= (nth 4 __row_list) "") (= (nth 5 __row_list) ""))
            (setq __error_str (strcat "请填写 " __key_str " 的材料性质及填充图案。")))
          (T (setq __keys_list (cons __key_str __keys_list)
              __positions_list (cons __position_list __positions_list)
              __strata_list (cons (list __order_int __sub_str __code_str
                (if (= (nth 3 __row_list) "0") "Cover" "Rock")
                (nth 4 __row_list) (strcase (nth 5 __row_list))) __strata_list)))))))
  (if (and (null __strata_list) (null __error_str)) (setq __error_str "至少填写一个地层。"))
  (if (null __error_str)
    (progn
      (setq __strata_list (vl-sort __strata_list 'project_info_strata_less))
      (foreach __row_list __strata_list
        (if (= (nth 3 __row_list) "Rock") (setq __rock_bool T)
          (if __rock_bool (setq __error_str "按层序排序后，覆盖层必须在岩层上方。"))))))
  (if __error_str __error_str __strata_list))

(defun project_info_page_save (self_page_int self_pages_list / __rows_list __i_int __suffix_str)
  (setq __i_int 0)
  (repeat 8
    (setq __suffix_str (itoa __i_int)
      __rows_list (cons (mapcar '(lambda (self_key_str) (vl-string-trim " \t" (get_tile (strcat self_key_str __suffix_str))))
        '("n" "s" "c" "k" "m" "p")) __rows_list)
      __i_int (1+ __i_int)))
  (cons (cons self_page_int (reverse __rows_list)) (vl-remove (assoc self_page_int self_pages_list) self_pages_list)))

(defun project_info_page_show (self_page_int self_pages_list / __rows_list __i_int __suffix_str)
  (setq __rows_list (cdr (assoc self_page_int self_pages_list)) __i_int 0)
  (repeat 8
    (setq __suffix_str (itoa __i_int))
    (mapcar '(lambda (self_key_str self_value_str) (set_tile (strcat self_key_str __suffix_str) self_value_str))
      '("n" "s" "c" "k" "m" "p") (if (car __rows_list) (car __rows_list) '("" "" "" "1" "" "ANSI31")))
    (setq __rows_list (cdr __rows_list) __i_int (1+ __i_int)))
  (set_tile "page" (strcat "第 " (itoa (1+ self_page_int)) " 页"))
  (set_tile "error" "")
  (mode_tile "previous" (if (= self_page_int 0) 1 0))
  (mode_tile "n0" 2))

(defun project_info_accept (self_pages_list / __result_any)
  (setq __result_any (project_info_validate_strata (apply 'append (mapcar 'cdr self_pages_list))))
  (if (= (type __result_any) 'STR)
    (progn (set_tile "error" __result_any) nil)
    (progn (done_dialog 1) __result_any)))

(defun project_info_catalog_dialog (self_id_int self_strata_list self_data_list / __pages_list __page_int __row_list __rows_list __key_str __i_int __result_list)
  (if (not (new_dialog "project_info_catalog" self_id_int))
    (project_info_fail "无法打开项目信息与地层表。"))
  (setq __page_int 0 __i_int 0)
  (foreach __row_list self_strata_list
    (setq __rows_list (cons (list (itoa (car __row_list)) (cadr __row_list) (caddr __row_list)
      (if (= (nth 3 __row_list) "Cover") "0" "1") (nth 4 __row_list) (nth 5 __row_list)) __rows_list))
    (if (= (length __rows_list) 8)
      (setq __pages_list (cons (cons __page_int (reverse __rows_list)) __pages_list)
        __rows_list nil __page_int (1+ __page_int))))
  (if __rows_list (setq __pages_list (cons (cons __page_int (reverse __rows_list)) __pages_list)))
  (setq __page_int 0)
  (mapcar 'set_tile '("project_title" "strata_title" "instructions"
      "order_header" "sub_header" "code_header" "kind_header" "material_header" "pattern_header")
    '("项目信息表" "地层表" "按层序编号和亚层序从上到下排列。亚层序可留空。"
      "层序编号" "亚层序" "地层代号" "类别" "材料性质" "填充图案代码"))
  (set_tile "summary" (project_info_gcd_summary self_data_list))
  (repeat 8
    (setq __key_str (strcat "k" (itoa __i_int)) __i_int (1+ __i_int))
    (start_list __key_str) (add_list "覆盖层") (add_list "岩石") (end_list))
  (project_info_page_show __page_int __pages_list)
  (action_tile "previous" "(setq __pages_list (project_info_page_save __page_int __pages_list) __page_int (1- __page_int))(project_info_page_show __page_int __pages_list)")
  (action_tile "next" "(setq __pages_list (project_info_page_save __page_int __pages_list) __page_int (1+ __page_int))(project_info_page_show __page_int __pages_list)")
  (action_tile "accept" "(setq __result_list (project_info_accept (project_info_page_save __page_int __pages_list)))")
  (if (= (start_dialog) 1) __result_list))

(defun project_info_root ( / __path_str)
  (cond
    ((and *project_info_root* (findfile (strcat *project_info_root* "/ProjectInfo.dcl"))) *project_info_root*)
    ((setq __path_str (or (findfile "ProjectInfo.dcl") (findfile "ProjectInfo/ProjectInfo.dcl")
        (findfile "W:/Code/LISP/AutoGeoDraw/ProjectInfo/ProjectInfo.dcl")
        (findfile "D:/OnedriveLS/OneDrive/Work/Code/LISP/AutoGeoDraw/ProjectInfo/ProjectInfo.dcl")))
      (setq *project_info_root* (vl-filename-directory __path_str)))
    (T (project_info_fail "找不到 ProjectInfo.dcl，请将 ProjectInfo 目录加入 CAD 支持路径。"))))

(defun project_info_edit (self_seed_list / __id_int __data_list __strata_list __result_any)
  (setq __data_list (project_info_gcd_stats)
    __strata_list (project_info_strata)
    __id_int (load_dialog (strcat (project_info_root) "/ProjectInfo.dcl")))
  (if (< __id_int 0) (project_info_fail "无法加载 ProjectInfo.dcl。"))
  (setq __result_any (vl-catch-all-apply 'project_info_catalog_dialog
    (list __id_int (if __strata_list __strata_list self_seed_list) __data_list)))
  (unload_dialog __id_int)
  (if (vl-catch-all-error-p __result_any)
    (project_info_fail (if *project_info_failure* *project_info_failure* (vl-catch-all-error-message __result_any))))
  (if __result_any
    (if (project_info_save __data_list __result_any) __result_any
      (project_info_fail "无法保存图纸项目表。"))))

(defun project_info_test ( / )
  (equal (project_info_validate_strata
    '(("10" "" "T1" "1" "Rock" "ansi32") ("2" "" "Q4" "0" "Soil" "ansi31")))
    '((2 "" "Q4" "Cover" "Soil" "ANSI31") (10 "" "T1" "Rock" "Rock" "ANSI32"))))

(defun project_info_start_program (self_refresh_bool / __data_list)
  (if self_refresh_bool
    (progn
      (setq __data_list (project_info_gcd_stats))
      (if (project_info_save __data_list (project_info_strata))
        (princ (strcat "\n" (project_info_gcd_summary __data_list)))
        (project_info_fail "无法保存图纸项目信息。")))
    (if (project_info_edit nil)
      (princ "\n已保存项目信息表和地层表。")
      (princ "\n已取消项目信息编辑。"))))

(defun project_info_run (self_refresh_bool / *project_info_failure* __result_any)
  (setq __result_any (vl-catch-all-apply 'project_info_start_program (list self_refresh_bool)))
  (if (vl-catch-all-error-p __result_any)
    (princ (strcat "\nProjectInfo：" (if *project_info_failure* *project_info_failure* (vl-catch-all-error-message __result_any)))))
  (princ))

(defun c:projectinfo ( / ) (project_info_run nil))
(defun c:projectgcd ( / ) (project_info_run T))
(princ)
