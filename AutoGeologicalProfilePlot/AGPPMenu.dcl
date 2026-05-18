StartMenu: dialog {
  : row {
    height= 5;
    width = 24;
    alignment = centered;
    children_alignment = centered;
    : text_part { 
      value =  "地质剖面自动绘制程序v1.0 Product By Liusha@2024";
     }
  }
  : row {
    fixed_height=true;
    fixed_width=true;
    height=3;
    width =24;
    children_alignment = centered;
    children_fixed_width = true;
    children_fixed_height = true;
    : column {
      : row {
        fixed_height=true;
        fixed_width=true;
        height=3;
        width =24;
        label = "选择平面图剖面线";
        : text { label = "无选择"; key = "SelectSectionLine"; value = "无选择"; }
        : retirement_button{ label = "选择"; key = "isSelectSectionLine";}
      }
      : row {
        fixed_height=true;
        fixed_width=true;
        height=3;
        width =24;
        label = "选择剖面图剖面线";
        : text { label = "无选择"; key = "SelectSectionGround"; value = "无选择"; }
        : retirement_button{ label = "选择"; key = "isSelectSectionGround";}
      }
    }
    : column {
      label = "图形比例";
    : row {
      : edit_box { 
        label = "剖面图比例1:";
        key = "PlanScale";
        value = "10000";
      }     
    }
    : row {
      : edit_box {  
        key = "SectionScale";
        label = "剖面图比例1:";
        value = "1000";
      }      
    }

    }
  }
  : row {
    fixed_height=true;
    fixed_width=true;
    height=3;
    width =24;
      label = "绘制";
    : retirement_button  { 
      label = "添加特征点";
      key = "AddFeaturePoint";
     }
    : retirement_button  { 
      label = "重新选择点";
      key = "RepickFeaturePoint";
     }
    : retirement_button  { 
      label = "删除上一点";
      key = "DeleteLastPoint";
     }
  }
  : row {
    height= 2;
    width = 24;
    alignment = centered;
    children_alignment = centered;
    : edit_box { key = "ImportBakupData"; label = "文件路径"; value = "NULL"; }
    : button { key = "BottonForImport"; label = "导入文件";}
  }
  : row {
    height= 2;
    width = 24;
    alignment = centered;
    children_alignment = centered;
    : edit_box { key = "OutputBakupData"; label = "文件路径"; value = "NULL"; }
    : button { key = "BottonForOutput"; label = "导出文件"; }
  }
  ok_cancel;
}

ConfirmEnd : dialog{
  : text { label = "是否结束选点？"; key = "ConfirmEndText"; value ="是否结束选点？";}
  ok_cancel;
}

ConfirmCancel : dialog{
  : text { label = "确定终止程序？";  key = "ConfirmCancelText"; value = "确定终止程序？";}
  ok_cancel;
}

/*
  地层下限输入框
*/
InputLimit : dialog {
  //第二行，第一列放线，第二列放正常蓄水位高程
      label = "层厚输入";
      fixed_height=true;
      fixed_width=true;
      height=3;
      width =24;

      : edit_box { 
        label = "位置描述：";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "location_name" ;
        value = "NULL";
      }

      : edit_box { 
        label = "土层厚度：";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "soil_limits" ;
        value = "3,";
      }
      : edit_box { 
        label = "强风化厚度";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "intense_limits";
        value = "3";
      }
      : edit_box { 
        label = "弱风化厚度";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "moderate_limits";
        value = "6";
      }
      //: retirement_button{ label = "添加";key = "add";}
  ok_cancel;
}

InputStructure : dialog {
  : row {      
    label = "地层线输入";
    fixed_height=true;
    fixed_width=true;
    children_fixed_width =true;
    children_fixed_height =true;
  : boxed_radio_column {
    label = "选择类型";
    alignment = left;
    width = 5;
    fixed_width = true;
    height = 2;
    fixed_height = true ;
    : radio_button {  //地层分界线；
      alignment = left;
      width = 8;
      fix_width = 8;
      label = "地层分界";
      key = "IsBoundary";
      value = 1;
      is_default = true ;
     }
    : radio_button {  //断层
      alignment = left;
      width = 10;
      fixed_width =10;
      label ="断层";
      key = "IsFault";
      value = 0;
     }
    : radio_button {  //褶皱
      alignment = left;
      width = 10;
      fixed_width =10;
      label = "褶皱";
      key = "IsFold";
      value =0;
     }
  }
  : boxed_column {
    label = "输入：";
    height = 2;
    children_fixed_height = 2 ;
    children_fixed_height = 2;
    children_fixed_width = 8;
    : edit_box{ key="StructurePointName";label="位置描述"; value = "NULL"; }
    : edit_box{ key="LastGeoCode";label="前面地层";  }
    : edit_box{ key="NextGeoCode";label="后面地层";  }
  }
}
  ok_cancel;
}

SelectPointType : dialog {
  : boxed_row {
    label = "特征点类型";
    : retirement_button { 
      fixed_width=true;
      width= 5;
      label ="分层厚度";
      key = "LimitsPoint";
    }
    : retirement_button { 
      fixed_width=true;
      width= 5;
      label ="地质构造";
      key = "StructurePoint";
    }
  }
  ok_cancel;
}

