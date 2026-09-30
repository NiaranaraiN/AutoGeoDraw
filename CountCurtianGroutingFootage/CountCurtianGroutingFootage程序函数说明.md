# CountCurtianGroutingFootage 目录 LSP 函数说明

`CountCurtainGroutingFootage.lsp` 负责帷幕灌浆进尺统计的数据处理和 DCL 输入。命令入口 `c:countcurtaingroutingfootage` 不作说明。

## CountCurtainGroutingFootage.lsp

| 函数 | 说明 |
|---|---|
| `bakup_env` | 保存当前对象捕捉模式和当前图层，供结束或出错时恢复。 |
| `recover_env` | 恢复 `bakup_env` 保存的对象捕捉模式与图层。 |
| `get_coorx` | 读取曲线起点和终点的 X 坐标，并按从小到大返回范围。 |
| `get_coorx_list` | 遍历选择集，收集每条曲线的 X 坐标范围；选择集为空时返回默认范围。 |
| `cuntain_start_point_coordinate` | 从选择集的曲线起点提取 X 或 Y 坐标，返回坐标列表。 |
| `caculate_real_data` | 根据图上读数、参考读数、实际参考值和比例换算实际高程/数值，并按精度取整。 |
| `get_inter_point` | 求两条图元曲线的交点，并将 ActiveX 返回值转换为坐标列表。 |
| `stratum_inter` | 按帷幕孔的 X 坐标筛选覆盖该位置的地层线，并返回孔线与地层线的交点。 |
| `stratum_length` | 将地层交点换算为实际高程，再与孔顶高程相减得到该地层的进尺；缺少有效交点时返回 0。 |
| `remove_overlap_length` | 将三类进尺按大小排序并计算相邻差值，扣除多类别重叠部分后按原类别顺序返回。 |
| `print_value_prop` | 在命令行打印一个值的数据类型和值。 |
| `test` | 调用重叠进尺处理函数并打印结果的测试入口。 |
| `start_program` | 主程序和 DCL 对话框的组织入口；包含控件回调和对话框流程。 |
| `action-format-keys` | 响应 DCL 控件事件，将帮助、确认、取消及各类图元选择操作映射为对话框返回码。 |
| `key-value-bak` | 收集当前 DCL 输入值，供对话框关闭或切换到图面选择前保存。 |
| `botton-action` | 根据对话框返回码继续显示界面或进入相应的图面选择流程。 |
| `get-input-value` | 读取对话框中的输入控件值并更新对应变量。 |
| `load-menu` | 加载统计界面 DCL，设置控件初值、注册事件并启动对话框。 |
| `*error*` | 命令执行出错时恢复环境变量并完成清理。 |

## TbxlsCore.lsp

| 函数 | 说明 |
|---|---|
| `vls-core-load` | 返回核心模块加载成功的状态文本，用于检查模块是否已加载。 |
| `vls-Http-Response` | 通过 `Msxml2.XMLHTTP` 向指定地址发起 GET 请求并返回响应文本，完成后释放 COM 对象。 |
| `vls-Modify-SystemVariant` | 返回程序运行时需要调整的系统变量名和值列表，包括尺寸显示、命令回显、当前图层和对象捕捉设置。 |
