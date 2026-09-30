# 帷幕灌浆进尺统计

本程序根据剖面图中的帷幕孔、正常蓄水位和各类地层/构筑物边界，计算各孔的分段进尺，并将结果写入 CSV 文件。

## 文件

| 文件 | 用途 |
|---|---|
| `CountCurtainGroutingFootage.lsp` | 主程序，提供 CAD 命令 `countcurtaingroutingfootage`。 |
| `CountCurtainGroutingFootageDCL.dcl` | 选择图元、输入比例与水位高程、设置输出路径的界面。 |
| `TbxlsCore.lsp` | 此程序目录中随附的核心辅助函数。 |
| `CountCurtianGroutingFootage程序函数说明.md` | 各非命令函数的用途说明。 |

## 使用流程

1. 在 CAD 中加载 `CountCurtainGroutingFootage.lsp`，并确保程序能找到 `TbxlsCore.lsp`。
2. 将主程序 `load-menu` 中的固定 `dialogFile` 路径改为当前 `CountCurtainGroutingFootageDCL.dcl` 的实际位置。
3. 输入命令 `countcurtaingroutingfootage`。在界面中选择帷幕孔线和正常蓄水位线，按需要选择地面线、土层线、坝体及混凝土边界。
4. 输入图纸比例、水位高程并选择 CSV 保存位置，确认后生成统计结果。

CSV 按孔逐行输出孔口/孔底高程、总进尺及土层、坝体、混凝土、基岩、有效和无效进尺等数值。程序使用图元交点和比例换算高程，输入图元的位置与比例会直接影响统计值。

详细函数说明见 [CountCurtianGroutingFootage程序函数说明.md](CountCurtianGroutingFootage程序函数说明.md)。
