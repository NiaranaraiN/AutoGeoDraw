sc_actions : dialog {
 label="SectionCutter";
 : text {key="summary"; width=76;}
 : list_box {key="actions"; width=76; height=19;}
 ok_cancel;
}
sc_form : dialog {
 label="SectionCutter 0.2";
 : text { key="title"; width=64; }
 : row { : text {key="l0"; width=26;} : edit_box {key="v0"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l1"; width=26;} : edit_box {key="v1"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l2"; width=26;} : edit_box {key="v2"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l3"; width=26;} : edit_box {key="v3"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l4"; width=26;} : edit_box {key="v4"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l5"; width=26;} : edit_box {key="v5"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l6"; width=26;} : edit_box {key="v6"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l7"; width=26;} : edit_box {key="v7"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l8"; width=26;} : edit_box {key="v8"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l9"; width=26;} : edit_box {key="v9"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l10"; width=26;} : edit_box {key="v10"; edit_width=32; edit_limit=2048;} }
 : row { : text {key="l11"; width=26;} : edit_box {key="v11"; edit_width=32; edit_limit=2048;} }
 : text {key="error"; width=64;}
 ok_cancel;
}
sc_strata : dialog {
 label="SectionCutter";
 : text {key="title"; width=54;}
 : row { : text {key="row_header"; width=24; fixed_width=true;} : text {key="value_header"; width=28;} }
 : row { : text {key="l0"; width=24; fixed_width=true;} : edit_box {key="v0"; edit_width=28;} }
 : row { : text {key="l1"; width=24; fixed_width=true;} : edit_box {key="v1"; edit_width=28;} }
 : row { : text {key="l2"; width=24; fixed_width=true;} : edit_box {key="v2"; edit_width=28;} }
 : row { : text {key="l3"; width=24; fixed_width=true;} : edit_box {key="v3"; edit_width=28;} }
 : row { : text {key="l4"; width=24; fixed_width=true;} : edit_box {key="v4"; edit_width=28;} }
 : row { : text {key="l5"; width=24; fixed_width=true;} : edit_box {key="v5"; edit_width=28;} }
 : row { : text {key="l6"; width=24; fixed_width=true;} : edit_box {key="v6"; edit_width=28;} }
 : row { : text {key="l7"; width=24; fixed_width=true;} : edit_box {key="v7"; edit_width=28;} }
 : row { : text {key="l8"; width=24; fixed_width=true;} : edit_box {key="v8"; edit_width=28;} }
 : row { : text {key="l9"; width=24; fixed_width=true;} : edit_box {key="v9"; edit_width=28;} }
 : row {
  : button {key="previous"; label="<"; fixed_width=true; width=8;}
  : text {key="page"; width=18; alignment=centered;}
  : button {key="next"; label=">"; fixed_width=true; width=8;}
 }
 : row {
  : text {key="water_label"; width=43;}
  : edit_box {key="water_depth"; edit_width=12;}
 }
 : text {key="weather_title"; width=60;}
 : row {
  : column { : text {key="wl0"; width=12;} : edit_box {key="w0"; edit_width=12;} }
  : column { : text {key="wl1"; width=12;} : edit_box {key="w1"; edit_width=12;} }
  : column { : text {key="wl2"; width=12;} : edit_box {key="w2"; edit_width=12;} }
  : column { : text {key="wl3"; width=12;} : edit_box {key="w3"; edit_width=12;} }
 }
 : text {key="error"; width=60;}
 ok_cancel;
}
sc_clean : dialog {
 label="SectionCutter - Clean unused data";
 : text {key="summary"; width=100;}
 : text {key="warning"; width=100;}
 : list_box {key="records"; width=100; height=18; multiple_select=true;}
 : row {
  : button {key="all"; label="Select all";}
  : button {key="none"; label="Clear selection";}
 }
 : text {key="error"; width=100;}
 ok_cancel;
}
