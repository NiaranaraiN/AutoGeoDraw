MainMenu: dialog {
  // 第一行，标题“灌浆工程量统计小程序v0.1.0_202405
  : boxed_row {
    fixed_width= true;
    fixed_height=true;
    height =5;
    width =60;
  }


  //第二行，第一列放线，第二列放正常蓄水位高程
  : row {
    // 二行一列，必选线段，
    : column {
      fixed_width= true;
      width=20;
      label ="必选线段";

      :row{
        : button { 
          fixed_width=true;
          width= 5;
          label ="选择灌浆线";
          key = "SSCurtainButtom";
          }
        : text { 
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          }
      }

      : row {
        : button { 
          label ="正常蓄水位";
          key = "SSWaterLevelButtom";
          fixed_width=true;
          width = 5;
        }
        : text { 
          value = "已选择 1";
          fixed_width=true;
           width = 10;
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
      }

      : edit_box { 
        label = "正常蓄水位:";
        fixed_width =true;
        width =10;
        edit_width = 8;
        alignment =left;
      }
    }

  }

  //第三行，备选线段集;
  : column {
    label ="可选线段";
    fixed_width = true;
    width = 50;
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
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          alignment=left;
          }        
          
          : button { 
          fixed_width=true;
          width= 5;
          label ="选择土底线";
          key = "SSSoilButtom";
          }
        : text { 
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          alignment=left;
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
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          alignment=left;
          }        
          
          : button { 
          fixed_width=true;
          width= 5;
          label ="选择坝底线";
          key = "SSDamBottomButtom";
          }
        : text { 
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          alignment=left;
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
          value = "已选择 999";
          fixed_width=true;
           width = 10;
          alignment=left;
          }        

          : spacer_1 { 
            fixed_width=true;
            width = 10;
           }
          
          : button { 
          fixed_width=true;
          width= 12;
          label ="重置所有选择";
          key = "SelectResetButtom";
          }
        // : text {
        //   value = "已选择 999";
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
    }

    : edit_box{
      fixed_width =true;
      width=40; 
      value="w:\\plugin\\";

    }
  }

  ok_cancel;
}