# 手工断面绘制

本程序在平面线与断面线之间换算点位，记录地层厚度和结构点，插值绘制地层线，并支持断面数据的备份与恢复。

## 文件与依赖

| 文件 | 用途 |
|---|---|
| `MSDMain.lsp` | 主程序和各 CAD 命令。 |
| `MSDView.dcl` | 当前仅为空白占位文件，主程序未加载它。 |
| `ManualSectionDraw程序函数说明.md` | 各非命令函数的用途说明。 |

加载 `MSDMain.lsp` 前，应让 CAD 能通过支持文件搜索路径找到 `TbxlsCore.lsp`、`TbxlsIO.lsp`、`TbxlsText.lsp` 和 `TbxlsGraphElement.lsp`。这些通用模块位于辅助函数目录 `vlsToolbox`。主程序加载时会自动显示一次命令帮助。

## 常用流程

1. 加载 `MSDMain.lsp` 后输入 `InitSectionVariants`，设置地层数量，选择平面与断面基准线、起终点以及两张图的比例。
2. 输入 `AddSectionPoint`，在断面线上取点并录入各层厚度；重复以建立控制点。
3. 输入 `DrawAllSectionLine`，对控制点插值并绘制各层界线。
4. 使用 `BakupSectionData` 保存当前断面数据，之后可用 `ImportSectionData` 恢复。备份包括主 `.vlsdata` 文件和同目录内的地层点、地面点文件，移动备份时应一起移动。

其他命令包括 `FDSA-find-section->plan`、`FDAS-find-plan->section`（平面与断面位置互查），`RSTRemoveSectionPoint`（删除断面点），以及 `ResetAllValue`（重置当前数据）。`AddPlanPoint` 目前直接调用 `add-section-point`，尚无独立的平面取点流程。

详细函数说明见 [ManualSectionDraw程序函数说明.md](ManualSectionDraw程序函数说明.md)。
