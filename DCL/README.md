# DCL 界面资源

本目录存放 `CurtainGroutingDCL.dcl`。文件定义了一个 `MainMenu` 对话框，包含灌浆线、正常蓄水位、地面线和其他可选边界的选择控件，以及数值输入区。

该目录只有界面定义，没有独立的 LSP 命令入口。主目录的 `CurtainGrouting.lsp` 当前没有调用 `load_dialog` 或引用此 DCL 文件，因此它目前作为界面草稿保存；单独加载 DCL 不会启动程序。

主目录另有 `countCGFootage.dcl`，而 `CountCurtianGroutingFootage` 小程序使用自己的 `CountCurtainGroutingFootageDCL.dcl`。修改界面时应先确认目标 LSP 实际加载的是哪个文件。
