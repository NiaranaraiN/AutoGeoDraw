; Read the current attributes on the picked plan entity. The saved contact is a section snapshot.
(defun sc_contact_kind (self_ent_ename self_pairs_list / __type_str __layer_str)
  (setq __type_str (sc_get "GeoType" self_pairs_list)
    __layer_str (cdr (assoc 8 (entget self_ent_ename))))
  (if (and (null (assoc "SectionRole" self_pairs_list))
      (member (cdr (assoc 0 (entget self_ent_ename))) '("LINE" "LWPOLYLINE" "POLYLINE")))
    (cond
      ((member __type_str '("地层分界线" "岩层分界线")) "Contact")
      ((member __type_str '("断层线" "断层")) "Fault")
      ((= __type_str "褶皱") "Fold")
      ((member __type_str '(nil ""))
        (sc_get __layer_str '(("Geo_地层分界线" . "Contact") ("Geo_岩层分界线" . "Contact")
          ("Geo_断层" . "Fault") ("Geo_褶皱" . "Fold")))))))

(defun sc_contact_pick_kind (self_ent_ename self_pairs_list self_data_list / __kind_str)
  (if (not (member self_ent_ename (list (sc_alive (sc_get 'plan self_data_list))
      (sc_alive (sc_get "terrain" (sc_get 'roles self_data_list))))))
    (cond
      ((sc_contact_kind self_ent_ename self_pairs_list))
      ((and (null (assoc "SectionRole" self_pairs_list))
          (member (sc_get "GeoType" self_pairs_list) '(nil ""))
          (member (cdr (assoc 0 (entget self_ent_ename))) '("LINE" "LWPOLYLINE" "POLYLINE")))
        "Unassigned"))))

(defun sc_contact_hits (self_ent_ename self_data_list / )
  (sc_plan_check self_data_list)
  (sc_intersections (sc_plan_geometry self_data_list) (sc_source_curve_read self_ent_ename)))

(defun sc_contact_pick_hit (self_pick_list self_data_list / *sc_failure* __hits_any __pick_list __hit_list __best_list __best_real __distance_real __tied_bool)
  (setq __hits_any (vl-catch-all-apply 'sc_contact_hits (list (car self_pick_list) self_data_list)))
  (sc_curve_cleanup)
  (cond
    ((vl-catch-all-error-p __hits_any)
      (princ (strcat "\n无法取得构造线交点：" (if *sc_failure* *sc_failure* (vl-catch-all-error-message __hits_any)))) nil)
    ((null __hits_any) (princ "\n此构造线与当前平面剖切线没有交点。") nil)
    (T
      (setq __pick_list (sc_flat (trans (cadr self_pick_list) 1 0)))
      (foreach __hit_list __hits_any
        (setq __distance_real (distance __pick_list (caddr __hit_list)))
        (cond
          ((and __best_real (equal __distance_real __best_real 1e-6)) (setq __tied_bool T))
          ((or (null __best_real) (< __distance_real __best_real))
            (setq __best_real __distance_real __best_list __hit_list __tied_bool nil))))
      (if __tied_bool (progn (princ "\n有多个等距交点，请靠近目标交点重新点选。") nil)
        __best_list))))

(defun sc_contact_source_at (self_ent_ename self_s_real self_data_list / *sc_failure* __pairs_list __kind_str __hits_any __hit_list)
  (if (and self_ent_ename
      (setq __kind_str (sc_contact_pick_kind self_ent_ename (setq __pairs_list (sc_pairs self_ent_ename)) self_data_list)))
    (progn
      (setq __hits_any (vl-catch-all-apply 'sc_contact_hits (list self_ent_ename self_data_list)))
      (sc_curve_cleanup)
      (if (not (vl-catch-all-error-p __hits_any))
        (if (setq __hit_list (vl-some '(lambda (self_hit_list)
            (if (equal (* (car self_hit_list) (sc_get 'p self_data_list)) self_s_real 1e-6) self_hit_list)) __hits_any))
          (list self_ent_ename __hit_list __pairs_list __kind_str))))))

(defun sc_contact_choose_source (self_s_real self_data_list self_prompt_str / __pick_list __source_list)
  (while (and (null __source_list) (setq __pick_list (entsel self_prompt_str)))
    (if (null (setq __source_list (sc_contact_source_at (car __pick_list) self_s_real self_data_list)))
      (princ "\n请选经过该交点的平面构造线。")))
  __source_list)

(defun sc_contact_feature_source (self_s_real self_data_list / __feature_list __ent_ename __source_list __sources_list)
  (foreach __feature_list (sc_get 'features self_data_list)
    (if (and (equal (car __feature_list) self_s_real 1e-6)
        (setq __ent_ename (sc_alive (caddr __feature_list)))
        (null (assoc __ent_ename __sources_list))
        (setq __source_list (sc_contact_source_at __ent_ename self_s_real self_data_list))
        (/= (nth 3 __source_list) "Unassigned"))
      (setq __sources_list (cons __source_list __sources_list))))
  (cond
    ((null (cdr __sources_list)) (car __sources_list))
    (T (if (setq __source_list (sc_contact_choose_source self_s_real self_data_list
        "\n该里程有多根构造线，请选择要读取的构造线（回车跳过此点）: "))
      __source_list 'skip))))

(defun sc_contact_attr_text (self_key_str self_pairs_list / __text_str)
  (if (and (setq __text_str (sc_get self_key_str self_pairs_list))
      (/= (setq __text_str (vl-string-trim " \t" __text_str)) ""))
    __text_str))

(defun sc_contact_attr_angle (self_key_str self_pairs_list self_max_real self_inclusive_bool / __text_str __value_real)
  (if (and (setq __text_str (sc_contact_attr_text self_key_str self_pairs_list))
      (setq __value_real (distof __text_str 2))
      (>= __value_real 0.0)
      (if self_inclusive_bool (<= __value_real self_max_real) (< __value_real self_max_real)))
    __value_real))

(defun sc_contact_trace_tangent (self_ent_ename self_hit_list / __point_list __param_real)
  (setq __point_list (vlax-curve-getClosestPointTo self_ent_ename
    (append (caddr self_hit_list) (list (cadr self_hit_list)))))
  (if (and __point_list (setq __param_real (vlax-curve-getParamAtPoint self_ent_ename __point_list))
      (> __param_real (+ (vlax-curve-getStartParam self_ent_ename) 1e-7))
      (< __param_real (- (vlax-curve-getEndParam self_ent_ename) 1e-7))
      (or (= (cdr (assoc 0 (entget self_ent_ename))) "LINE")
        (not (equal __param_real (float (fix (+ __param_real 0.5))) 1e-7))))
    (vlax-curve-getFirstDeriv self_ent_ename __param_real)))

(defun sc_contact_upper_side (self_ent_ename self_hit_list self_s_real self_az_real self_data_list / *sc_failure* __tangent_any __bearing_any __normal_list __size_real __upper_real __forward_real)
  (if self_az_real
    (progn
      (setq __tangent_any (vl-catch-all-apply 'sc_contact_trace_tangent (list self_ent_ename self_hit_list))
        __bearing_any (vl-catch-all-apply 'sc_bearing (list self_s_real self_data_list)))
      (if (and (not (vl-catch-all-error-p __tangent_any)) __tangent_any
          (not (vl-catch-all-error-p __bearing_any)))
        (progn
          (setq __normal_list (list (- (cadr __tangent_any)) (car __tangent_any))
            __size_real (sqrt (sc_dot __normal_list __normal_list)))
          (if (> __size_real 1e-10)
            (progn
              (setq __normal_list (sc_vscale __normal_list (/ 1.0 __size_real))
                self_az_real (* pi (/ self_az_real 180.0))
                __bearing_any (* pi (/ __bearing_any 180.0))
                __upper_real (sc_dot __normal_list (list (sin self_az_real) (cos self_az_real)))
                __forward_real (sc_dot __normal_list (list (sin __bearing_any) (cos __bearing_any))))
              (if (and (> (abs __upper_real) 1e-6) (> (abs __forward_real) 1e-6))
                (if (> (* __upper_real __forward_real) 0.0) 'after 'before)))))))))

(defun sc_contact_attr_values (self_pairs_list self_upper_sym / __name_str __upper_list __lower_list)
  (if (null (setq __name_str (sc_contact_attr_text "GeoEntityName" self_pairs_list)))
    (setq __name_str (sc_contact_attr_text "GeoType" self_pairs_list)))
  (setq __upper_list (list (sc_contact_attr_text "上层" self_pairs_list)
      (sc_contact_attr_angle "上层倾向" self_pairs_list 360.0 nil)
      (sc_contact_attr_angle "上层倾角" self_pairs_list 90.0 T))
    __lower_list (list (sc_contact_attr_text "下层" self_pairs_list)
      (sc_contact_attr_angle "下层倾向" self_pairs_list 360.0 nil)
      (sc_contact_attr_angle "下层倾角" self_pairs_list 90.0 T)))
  (append (list __name_str)
    (cond ((eq self_upper_sym 'before) (append __upper_list __lower_list))
      ((eq self_upper_sym 'after) (append __lower_list __upper_list))
      (T '(nil nil nil nil nil nil)))
    (list (sc_contact_attr_angle "构造面倾向" self_pairs_list 360.0 nil)
      (sc_contact_attr_angle "构造面倾角" self_pairs_list 90.0 T))))

(defun sc_contact_has_value (self_value_any / )
  (or (numberp self_value_any)
    (and (= (type self_value_any) 'STR) (/= (vl-string-trim " \t" self_value_any) ""))))

(defun sc_contact_attributes_complete (self_pairs_list / )
  (and (sc_contact_attr_angle "上层所在方位" self_pairs_list 360.0 nil)
    (vl-every 'sc_contact_has_value (sc_contact_attr_values self_pairs_list 'before))))

(defun sc_contact_ask_kind (self_default_str / __kind_str)
  (if (= self_default_str "Unassigned") (setq self_default_str nil))
  (if self_default_str (initget "Contact Fault Fold") (initget 1 "Contact Fault Fold"))
  (setq __kind_str (getkword (strcat "\n选择构造线类型 [地层分界Contact/断层Fault/褶皱Fold]"
    (if self_default_str (strcat "<" self_default_str ">") "") ": ")))
  (if __kind_str __kind_str self_default_str))

(defun sc_contact_assign (self_source_list self_kind_str / __file_str __path_str __template_list __pairs_list __field_list __keys_list __defaults_list __values_list __read_kind_str)
  (setq __file_str (sc_get self_kind_str '(("Contact" . "stratigraphic_boundary.vgtpl")
      ("Fault" . "fault_line.vgtpl") ("Fold" . "fold.vgtpl")))
    __path_str (findfile (strcat *sc_toolbox_root* "/vlsgeoconfig/templates/" __file_str)))
  (if (null __path_str) (setq __path_str (vls_geo_template_path __file_str)))
  (if (or (null __path_str) (null (setq __template_list (vls_io_read_template __path_str))))
    (sc_fail (strcat "无法读取构造线属性模板 " __file_str "。")))
  (setq __pairs_list (nth 2 self_source_list))
  (foreach __field_list __template_list
    (if (null (assoc (car __field_list) __pairs_list))
      (setq __pairs_list (append __pairs_list (list __field_list)))))
  (if (/= self_kind_str (nth 3 self_source_list))
    (setq __pairs_list (sc_put "GeoType" (sc_get "GeoType" __template_list) __pairs_list)))
  (setq __keys_list '("GeoEntityName" "上层" "上层倾向" "上层倾角" "下层" "下层倾向" "下层倾角"
      "上层所在方位" "构造面倾向" "构造面倾角")
    __defaults_list (mapcar '(lambda (self_key_str)
      (if (sc_get self_key_str __pairs_list) (sc_get self_key_str __pairs_list) "")) __keys_list)
    __values_list (sc_form "赋构造线属性（方位从正北顺时针，角度单位为度）"
      (mapcar '(lambda (self_field_list self_default_str)
        (list (car self_field_list) self_default_str (cadr self_field_list)))
        '(("构造名称或代号" text) ("上层完整编号" text) ("上层倾向" azimuth) ("上层倾角" dip)
          ("下层完整编号" text) ("下层倾向" azimuth) ("下层倾角" dip) ("上层所在方位" azimuth)
          ("构造面倾向" azimuth) ("构造面倾角" dip)) __defaults_list)))
  (mapcar '(lambda (self_key_str self_value_any)
    (setq __pairs_list (sc_put self_key_str (if (numberp self_value_any) (rtos self_value_any 2 15)
      (vl-string-trim " \t" self_value_any)) __pairs_list))) __keys_list __values_list)
  (setq __pairs_list (sc_put "TplVersion" (sc_get "TplVersion" __template_list) __pairs_list))
  (if (not (sc_contact_attributes_complete __pairs_list)) (sc_fail "请填写完整的构造线属性。"))
  (if (not (vls_geo_attributes_write_template (car self_source_list) __pairs_list))
    (sc_fail "无法将地质属性写入所选构造线。"))
  (setq *sc_mutated* T)
  (setq __pairs_list (sc_pairs (car self_source_list))
    __read_kind_str (sc_contact_kind (car self_source_list) __pairs_list))
  (if (or (/= __read_kind_str self_kind_str) (not (sc_contact_attributes_complete __pairs_list)))
    (sc_fail "构造线属性写入校验失败。"))
  (list (car self_source_list) (cadr self_source_list) __pairs_list __read_kind_str))

(defun sc_contact_form (self_defaults_list / )
  (sc_form "交点两侧真实产状与构造面产状（前=左，后=右）"
    (mapcar '(lambda (self_field_list self_value_any)
      (list (car self_field_list)
        (cond ((numberp self_value_any) (sc_fmt self_value_any))
          ((sc_contact_has_value self_value_any) self_value_any) (T ""))
        (cadr self_field_list)))
      '(("构造名称或代号" text) ("前侧地层代号" text) ("前侧倾向" azimuth) ("前侧倾角" dip)
        ("后侧地层代号" text) ("后侧倾向" azimuth) ("后侧倾角" dip)
        ("构造面倾向" azimuth) ("构造面倾角" dip)) self_defaults_list)))

(defun sc_contact_read_source (self_source_list self_s_real self_data_list / __pairs_list __upper_sym __choice_str __values_list)
  ; Source tuple contains the entity, exact hit, XData pairs, and structure kind.
  (setq __pairs_list (nth 2 self_source_list)
    __upper_sym (sc_contact_upper_side (car self_source_list) (cadr self_source_list) self_s_real
      (sc_contact_attr_angle "上层所在方位" __pairs_list 360.0 nil) self_data_list))
  (if (null __upper_sym)
    (progn
      (princ "\n交点线形无法唯一确定前后侧。")
      (initget "Before After")
      (setq __choice_str (getkword "\n上层位于 [剖面前侧Before/剖面后侧After]（回车跳过此点）: ")
        __upper_sym (sc_get __choice_str '(("Before" . before) ("After" . after))))))
  (if __upper_sym
    (progn
      (setq __values_list (sc_contact_attr_values __pairs_list __upper_sym))
      (princ (strcat "\n采用构造线属性：" (car __values_list) "，里程 " (sc_fmt self_s_real)
        " m，前侧 " (nth 1 __values_list) "，后侧 " (nth 4 __values_list) "。"))
      __values_list)))

(defun sc_contact_edit (self_data_list / __contacts_list __input_list __s_real __kind_str __values_list __contact_list __old_list __row_list __source_list __handle_str __old_handle_str __feature_list __assign_bool __skip_bool)
  (setq __contacts_list (sc_get 'contacts self_data_list))
  (foreach __feature_list (sc_get 'features self_data_list)
    (princ (strcat "\n交点 " (sc_fmt (car __feature_list)) "m：" (cadr __feature_list))))
  (princ "\n点选平面构造线，靠近要录入的交点；也可手工录入。")
  (while (setq __input_list (sc_control_station_input self_data_list __contacts_list 'contacts))
    (setq __s_real (car __input_list) __source_list (cadr __input_list)
      __row_list (sc_control_row_at __s_real __contacts_list) __old_list (cdr __row_list)
      __old_handle_str (sc_get 'source __old_list) __values_list nil)
    (if __row_list (setq __s_real (car __row_list)))
    (if (and (null __source_list) (null (caddr __input_list)))
      (setq __source_list (sc_contact_feature_source __s_real self_data_list)))
    (setq __skip_bool (eq __source_list 'skip))
    (if __skip_bool (setq __source_list nil))
    (setq __assign_bool (and __source_list (or (= (nth 3 __source_list) "Unassigned")
      (not (sc_contact_attributes_complete (nth 2 __source_list))))))
    (if __assign_bool
      (setq __source_list (sc_contact_choose_source __s_real self_data_list
        "\n交点构造线属性缺失或无效，请选择要赋值的构造线（回车跳过此点）: ")))
    (setq __handle_str (if __source_list (cdr (assoc 5 (entget (car __source_list)))) __old_handle_str))
    (cond
      (__source_list
        (if (or (null __old_handle_str) (= __handle_str __old_handle_str)
            (sc_yes "\n该里程已有另一构造线的交点。用本次点选的构造线替换？"))
          (progn
            (if __assign_bool
              (setq __source_list (sc_contact_assign __source_list (sc_contact_ask_kind (nth 3 __source_list)))))
            (setq __kind_str (nth 3 __source_list)
              __values_list (sc_contact_read_source __source_list __s_real self_data_list)))))
      ((not (or __assign_bool __skip_bool))
        (setq __kind_str (sc_contact_ask_kind (if __old_list (sc_get 'kind __old_list) "Contact"))
          __values_list (sc_contact_form
            (if __old_list (mapcar '(lambda (self_key_sym) (sc_get self_key_sym __old_list))
              '(name left la ld right ra rd az dip))
              '("分界1" "T1" 135.0 30.0 "T2" 135.0 30.0 135.0 45.0))))))
    (if __values_list
      (progn
        (setq __contact_list (list (cons 's __s_real) (cons 'kind __kind_str)))
        (if __handle_str (setq __contact_list (sc_put 'source __handle_str __contact_list)))
        (mapcar '(lambda (self_key_sym self_value_any) (setq __contact_list (sc_put self_key_sym self_value_any __contact_list)))
          '(name left la ld right ra rd az dip) __values_list)
        (setq __contacts_list (sc_put __s_real __contact_list __contacts_list)))))
  (sc_put 'contacts (sc_ci_sort __contacts_list) self_data_list))
