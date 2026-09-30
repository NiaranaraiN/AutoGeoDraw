# AutoGeologicalProfilePlot 目录 LSP 函数说明

本目录包含地质剖面绘制主程序、开发测试副本和临时辅助文件。`C:` 开头的是 AutoCAD 命令入口，按要求不逐项说明。`test.lsp` 与主程序包含大量同名实现，下面按函数名合并说明，便于查阅。

## DCL 界面与输入

| 函数 | 说明 |
|---|---|
| `select-feature-point-type` | 显示地质特征点类型选择对话框，并根据选择进入地层界限点或结构点输入。 |
| `sfpt-set-all-key-value` | 收集特征点类型对话框中各控件的当前值。 |
| `sfpt-action-tile->function` | 处理特征点类型对话框的取消、确认、帮助及类型选择事件。 |
| `sfpt-run-function-by-dialog-id` | 根据特征点类型对话框返回码继续流程，并记录所选点类别。 |
| `sfpt-send-global-value` | 为对话框值传递预留的辅助函数；当前实现为空。 |
| `sfpt-load-menu-window` | 加载并显示特征点类型 DCL，初始化控件并分派对话框返回结果。 |
| `input-stratum-limits-point` | 显示地层界限点输入界面，收集位置名称、各层厚度及风化层参数，并追加平面点。 |
| `isl-key-value-get` | 收集地层界限点表单控件值。 |
| `isl-action-format-keys` | 响应地层界限点表单的取消、确认和各输入控件事件。 |
| `isl-botton-action` | 根据表单返回码返回主菜单，或将输入的界限点信息追加到全局数据列表。 |
| `isl-load-menu` | 装载地层界限点 DCL，初始化控件及默认点名，循环处理用户输入。 |
| `input-structure-point` | 显示结构点输入界面，记录结构类型、名称、前后地质编号及平面点。 |
| `isp-set-all-key-value` | 收集结构点对话框控件的当前值。 |
| `isp-action-tile->function` | 将结构点对话框控件事件映射为对话框返回码。 |
| `isp-run-function-by-dialog-id` | 根据结构点类型或确认结果保存结构点字段，并选择后续界面。 |
| `isp-send-global-value` | 为结构点控件值传递预留的辅助函数；当前实现为空。 |
| `isp-load-menu-window` | 装载结构点 DCL、填充控件并循环分派交互结果。 |
| `menu-confrim-end` | 显示结束确认对话框；取消时返回主菜单，确认时结束点位输入流程。 |
| `mce-action-tile->function` | 处理结束确认对话框的取消和确认事件。 |
| `mce-run-function-by-dialog-id` | 根据结束确认返回码返回主菜单或继续结束流程。 |
| `mce-load-menu-window` | 加载结束确认 DCL、初始化提示文字并启动对话框。 |
| `menu-confrim-cancel` | 显示取消操作确认对话框；取消时回到主菜单，确认时终止当前对话流程。 |
| `mcc-action-tile->function` | 处理取消确认对话框的取消和确认事件。 |
| `mcc-run-function-by-dialog-id` | 根据取消确认结果返回主菜单或终止对话流程。 |
| `mcc-load-menu-window` | 加载取消确认 DCL、初始化提示文字并分派用户选择。 |
| `start-main-menu` | 显示主菜单并管理特征点采集、平面线/断面线选择、比例设置以及状态导入导出。 |
| `smm-set-all-key-value` | 收集主菜单中所有已注册控件的当前值。 |
| `smm-action-tile->function` | 处理主菜单各按钮和比例控件事件，将操作转换为返回码并更新比例值。 |
| `smm-run-function-by-dialog-id` | 执行主菜单选择后的逻辑，包括取点、选择控制线、增删点、恢复状态和保存状态。 |
| `smm-output-dcl-value` | 将变量值转为 DCL 可显示文本；对列表取末项，对字符串、整数和实数转换为文本。 |
| `smm-send-global-value` | 为主菜单控件值传递预留的辅助函数；当前实现为空。 |
| `smm-load-menu-window` | 加载主菜单 DCL、填充控件、启动对话框并将返回结果交给菜单分派函数。 |

## 几何与地层剖面

| 函数 | 说明 |
|---|---|
| `get-start-point-coefficient` | 计算指定点沿所选平面线到终点方向的距离，用于对齐平面与断面的起点方向。 |
| `get-limits-index-lists` | 计算各地层界限点沿平面线的距离，并按距离排序，得到相邻界限点索引。 |
| `make-polyline` | 根据三维点列表创建轻量多段线并返回新实体名。 |
| `make-sectionwith-line` | 在给定 X 坐标创建一条上下延伸的竖向临时线。 |
| `get-point-at-line` | 求指定多段线与另一条图元的交点并返回交点坐标；未找到交点时提示并退出。 |
| `get-draw-limits-lists` | 按界限点、比例和地层参数计算各地层界线及风化界线的绘图坐标。 |
| `input-draw-line-value` | 将某个剖面位置的地面高程、各层厚度和风化厚度累积为多条界线的坐标数据。 |
| `add-draw-limits-value` | 读取相邻界限点的土层与风化参数，按水平距离插值并追加中间绘图坐标。 |
| `start-get-draw-limits-lists` | 初始化绘图起点和临时剖面线，遍历所有界限区间并生成最终边界坐标。 |
| `draw-soil-line` | 调用界限坐标计算函数并尝试绘制多段线；当前循环计数器未更新，流程可能无法结束。 |

## 程序状态与调试辅助

| 函数 | 说明 |
|---|---|
| `init-variant` | 初始化 DCL 文件路径、变量默认值和需要保存/恢复的全局变量名称列表。 |
| `set-variant` | 根据输入比例计算平面/剖面换算系数，确定界限点顺序并启动剖面坐标计算。 |
| `print-variant` | 遍历变量名列表并在命令行打印变量值，供调试检查。 |
| `start-program` | 主流程入口：初始化状态、显示主菜单、换算比例并绘制剖面地层线。 |
| `*error*` | 命令入口的错误处理回调，用于恢复先前保存的系统变量并结束撤销标记。 |

## 文件范围

以上函数主要定义在 `AutoGeologicalProfilePlot.lsp`，同名实现也出现在 `test.lsp`。`temp.lsp` 只有命令入口 `c:remove-last-point`，按要求不作说明。
