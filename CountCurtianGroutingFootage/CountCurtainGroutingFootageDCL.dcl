MainMenu: dialog {
  // 第一行，标题“灌浆工程量统计小程序v0.1.0_202405
    : boxed_column {
      fixed_height =true;
      height =2;
  //width =60;
      alignment = centered;
    // : image { 
    //   width=60;
    //   height=3;
    //   key="imgTitle";
    //   color=7;
    // }
    : text { 
      // fixed_height =true;
      // fixed_width = true;
      // height = 2;
      // width = 60;
      value = "AimoyuRen@灌浆工程量统计程序 v0.1.0.202405";
      alignment =centered;
      }
    }


  //第二行，第一列放线，第二列放正常蓄水位高程
  : row {
    fixed_height =true;
    height = 4;
    // 二行一列，必选线段，
    : column {
      fixed_width= true;
      width=15;

      label ="必选线段";

      :row{
        : button { 
          fixed_width=true;
          width= 5;
          label ="选择灌浆线";
          key = "SSCurtainButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          alignment =left;
          key ="SelectCurtainStatus";
          }
      }

      : row {
        : button { 
          label ="正常蓄水位";
          key = "SSWaterLevelButtom";
          fixed_width=true;
          width = 5;
          alignment =left;
        }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          key="SelectWaterLevelStatus";
        }
      }
    }

    : boxed_column { 
      label = "数值输入";
      fixed_height=true;
      fixed_width=true;
      height=3;
      width =24;

      : edit_box { 
        label = "图纸比例  1:";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "drawingScaleInput" ;
        value = "1000";
      }

      : edit_box { 
        label = "正常蓄水位:";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
        key = "WaterLevelElevationInput";
        value = "1450";
      }
    }

  }

  //第三行，可选线段集;
  : column {
    label ="可选线段";
    fixed_width = true;
    width = 55;
    fixed_height =true;
    height =6;
      :row{
        fixed_width =true;
        width =20;

        : button { 
          fixed_width=true;
          width= 5;
          label ="选择地面线";
          key = "SSGroundButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          alignment=left;
          key = "SelectGroundStatus";
          }        
          
          : spacer_1 { 
            fixed_width=true;
            width = 4;
           }


          : button { 
          fixed_width=true;
          width= 5;
          label ="选择土底线";
          key = "SSSoilButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          alignment=left;
          key= "SelectSoilStatus";
          }

      }

      :row{
        fixed_width =true;
        width =20;

        : button { 
          fixed_width=true;
          width= 5;
          label ="选择坝顶线";
          key = "SSDamTopButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
           width = 10;
          alignment=left;
          key = "SelectDamTopStatus";
          }        

          : spacer_1 { 
            fixed_width=true;
            width= 4 ;
           }


          : button { 
          fixed_width=true;
          width= 5;
          label ="选择坝底线";
          key = "SSDamBottomButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          alignment=left;
          key = "SelectDamBottomStatus";
          }
      }

      :row{
        fixed_width =true;
        width =20;

        : button { 
          fixed_width=true;
          width= 5;
          label ="选择其它线";
          key = "SSConcreteButtom";
          }
        : text { 
          value = "未选择";
          fixed_width=true;
          width = 10;
          alignment=left;
          key = "SelectConcreteStatus";
          }

          : spacer_1 { 
            fixed_width=true;
            width = 13;
          }
          
          : button { 
          fixed_width=true;
          width= 12;
          label ="重置所有选择";
          key = "SelectResetButtom";
          }
        // : text {
        //   value = "未选择";
        //   fixed_width=true;
        //    width = 10;
        //   alignment=left;
        //   }
      }

  }


  : row{
    alignment=left;
    fixed_width =true;
    width=50;
    : button{ 
      label="保存路径";
      fixed_width=true;
      width=4;
      key = "SavePathButtom";
    }

    : edit_box{
      fixed_width =true;
      width=47; 
      value="请选择保存路径！";
      key = "ShowSavePath";
    }
  }

  ok_cancel_help;
}