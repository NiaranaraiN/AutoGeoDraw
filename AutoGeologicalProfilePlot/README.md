# 地质剖面图自动绘制

本目录用于在平面图和剖面图之间关联地质特征点，录入地层厚度、风化层厚度及地质结构信息，并据此生成剖面界线。当前代码仍处于开发阶段。

## 文件

| 文件 | 用途 |
|---|---|
| `AutoGeologicalProfilePlot.lsp` | 主程序，定义 CAD 命令 `TT`、DCL 交互和剖面线计算逻辑。 |
| `AGPPMenu.dcl` | 主菜单、结束/取消确认、界限点、结构点和特征点类型选择界面。 |
| `test.lsp` | 包含大量与主程序同名函数的开发测试副本。 |
| `temp.lsp` | 临时命令 `remove-last-point`。 |
| `程序设计.md` | 早期设计思路。 |
| `AutoGeologicalProfilePlot程序函数说明.md` | 各非命令函数的用途说明。 |

## 预期使用流程

1. 在 CAD 中加载主 LSP 文件，并使 `TbxlsCore.lsp`、`TbxlsText.lsp` 可通过支持文件搜索路径找到。
2. 将主程序中 `DCL_PATH` 的固定路径改为实际 `AGPPMenu.dcl` 路径。
3. 输入命令 `TT`，在界面中选择平面剖面线与剖面地面线、设置比例、添加地层界限点和结构点。
4. 确认后由程序计算地层与风化界线坐标并尝试绘制多段线。

## 当前状态

`start-program` 仍写入固定的测试数据。`draw-soil-line` 的循环没有更新计数器，且末尾再次绘制的是强风化线；`print-variant` 在主流程中未传入它要求的参数。这些代码路径可能使 `TT` 无法正常完成绘图。使用前应先修正并在 CAD 中验证。

主程序与 `test.lsp` 定义了多组同名函数；在同一 CAD 会话中加载两个文件时，后加载的定义会覆盖先加载的定义。

详细函数说明见 [AutoGeologicalProfilePlot程序函数说明.md](AutoGeologicalProfilePlot程序函数说明.md)。
