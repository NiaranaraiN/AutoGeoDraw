project_info_catalog : dialog {
 label="ProjectInfo";
 : boxed_column {
  : text {key="project_title"; width=109;}
  : text {key="summary"; width=109;}
 }
 : boxed_column {
  : text {key="strata_title"; width=109;}
  : text {key="instructions"; width=109;}
  : row {
   : text {key="order_header"; width=10; fixed_width=true;}
   : text {key="sub_header"; width=13; fixed_width=true;}
   : text {key="code_header"; width=18; fixed_width=true;}
   : text {key="kind_header"; width=17; fixed_width=true;}
   : text {key="material_header"; width=28; fixed_width=true;}
   : text {key="pattern_header"; width=19; fixed_width=true;}
  }
  : row { : edit_box {key="n0"; edit_width=9; fixed_width=true;} : edit_box {key="s0"; edit_width=12; fixed_width=true;} : edit_box {key="c0"; edit_width=17; fixed_width=true;} : popup_list {key="k0"; width=17; fixed_width=true;} : edit_box {key="m0"; edit_width=27; fixed_width=true;} : edit_box {key="p0"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n1"; edit_width=9; fixed_width=true;} : edit_box {key="s1"; edit_width=12; fixed_width=true;} : edit_box {key="c1"; edit_width=17; fixed_width=true;} : popup_list {key="k1"; width=17; fixed_width=true;} : edit_box {key="m1"; edit_width=27; fixed_width=true;} : edit_box {key="p1"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n2"; edit_width=9; fixed_width=true;} : edit_box {key="s2"; edit_width=12; fixed_width=true;} : edit_box {key="c2"; edit_width=17; fixed_width=true;} : popup_list {key="k2"; width=17; fixed_width=true;} : edit_box {key="m2"; edit_width=27; fixed_width=true;} : edit_box {key="p2"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n3"; edit_width=9; fixed_width=true;} : edit_box {key="s3"; edit_width=12; fixed_width=true;} : edit_box {key="c3"; edit_width=17; fixed_width=true;} : popup_list {key="k3"; width=17; fixed_width=true;} : edit_box {key="m3"; edit_width=27; fixed_width=true;} : edit_box {key="p3"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n4"; edit_width=9; fixed_width=true;} : edit_box {key="s4"; edit_width=12; fixed_width=true;} : edit_box {key="c4"; edit_width=17; fixed_width=true;} : popup_list {key="k4"; width=17; fixed_width=true;} : edit_box {key="m4"; edit_width=27; fixed_width=true;} : edit_box {key="p4"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n5"; edit_width=9; fixed_width=true;} : edit_box {key="s5"; edit_width=12; fixed_width=true;} : edit_box {key="c5"; edit_width=17; fixed_width=true;} : popup_list {key="k5"; width=17; fixed_width=true;} : edit_box {key="m5"; edit_width=27; fixed_width=true;} : edit_box {key="p5"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n6"; edit_width=9; fixed_width=true;} : edit_box {key="s6"; edit_width=12; fixed_width=true;} : edit_box {key="c6"; edit_width=17; fixed_width=true;} : popup_list {key="k6"; width=17; fixed_width=true;} : edit_box {key="m6"; edit_width=27; fixed_width=true;} : edit_box {key="p6"; edit_width=18; fixed_width=true;} }
  : row { : edit_box {key="n7"; edit_width=9; fixed_width=true;} : edit_box {key="s7"; edit_width=12; fixed_width=true;} : edit_box {key="c7"; edit_width=17; fixed_width=true;} : popup_list {key="k7"; width=17; fixed_width=true;} : edit_box {key="m7"; edit_width=27; fixed_width=true;} : edit_box {key="p7"; edit_width=18; fixed_width=true;} }
  : row {
   : button {key="previous"; label="<"; fixed_width=true; width=8;}
   : text {key="page"; width=18; alignment=centered;}
   : button {key="next"; label=">"; fixed_width=true; width=8;}
  }
 }
 : text {key="error"; width=109;}
 ok_cancel;
}
