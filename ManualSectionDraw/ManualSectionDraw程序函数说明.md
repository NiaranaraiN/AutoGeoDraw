# ManualSectionDraw 目录 LSP 函数说明

`MSDMain.lsp` 实现断面与平面之间的点位换算、地层线绘制、数据备份恢复和绘图状态管理。各 `C:` 开头的函数是可直接调用的命令入口，按要求不逐项解释。

## 状态、几何与点位

| 函数 | 说明 |
|---|---|
| `vls-Modify-SystemVariant` | 返回命令执行期间要修改的系统变量列表，供命令入口记录并在结束时恢复。 |
| `draw-strata-height-line-now` | 从给定地面点按地层厚度列表逐层向下计算边界，并按图层和颜色绘制线段、将线段编组。 |
| `find-section->plan` | 让用户在断面或平面线上选点，换算另一侧对应位置并绘制辅助投影线。 |
| `find-plan->section` | 让用户在平面线上选点，换算对应断面位置并绘制辅助投影线。 |
| `line-intersectionwith` | 计算两条 VLA 曲线的交点并返回坐标结果。 |
| `section-distance->plan-point` | 将断面线上相对起点的距离按比例换算为平面线上的对应点。 |
| `plan-distance->section-distance` | 将平面点换算为相对于断面起点的断面距离。 |
| `plan-point->section-point` | 将平面点投影到断面坐标，结合平面线、断面线及交点确定断面位置。 |
| `get-remove-element-index` | 查找列表中与指定待删除元素匹配的位置，并返回剩余元素的索引信息。 |
| `make-xline` | 以中心点和方向向量创建构造线，并返回创建结果。 |
| `points-draw-polyline` | 根据点列表创建多段线，并返回实体结果。 |

## 断面数据与绘制

| 函数 | 说明 |
|---|---|
| `vls-obj-list->groupunname` | 将对象列表编成匿名组，并返回组对象或组信息。 |
| `vls-obj-list->named-group` | 将对象列表编入指定名称的组。 |
| `vls-string->list` | 按指定分隔符拆分字符串并返回列表。 |
| `reset-all-value` | 清空或重置断面绘图使用的全局数据和状态变量。 |
| `append-in-section-data` | 将指定地面点和地层厚度追加到断面数据集合。 |
| `add-section-point` | 在断面线上选取位置、输入地层厚度，计算对应平面位置并保存/绘制断面点。 |
| `add-plan-point` | 当前仅调用 `add-section-point`，尚无独立的平面取点处理。 |
| `add-structure-point` | 采集并记录构造物或地质结构点及其关联信息。 |
| `select-section-lines` | 采集地层数量，选择平面与断面基准线及其起终点，并读取比例、计算换算系数。 |
| `remove-section-point` | 根据用户选取的位置定位并删除对应的断面点数据及相关图元。 |
| `strata-points-Interpolation` | 对地层点序列进行插值处理，生成需要补充的中间点。 |
| `insert-increment-points` | 根据当前点、下一点及地面控制点计算两点之间的插值点。 |
| `get-points-Interpolation-list` | 遍历并排序输入点，生成相邻地层点之间的插值点列表。 |
| `draw-all-section-line` | 按已保存的断面数据批量绘制各条地层线。 |

## 备份与恢复

| 函数 | 说明 |
|---|---|
| `bakup-variants->file-line` | 将指定变量序列化为带类型标记的一行并写入备份文件；支持整数、实数、字符串、实体、VLA 对象和列表。 |
| `import-variants<-file-line` | 解析带类型标记的变量行，并将数据恢复为对应 AutoLISP 类型。 |
| `import-section-data` | 选择备份数据文件，恢复程序变量及分拆保存的地层点文件。 |
| `bakup-section-data` | 将断面变量和各地层点数据写入备份文件。 |

## 错误处理

| 函数 | 说明 |
|---|---|
| `*error*` | 各命令入口定义的错误回调，用于恢复命令执行前保存的系统变量并结束撤销标记。 |
