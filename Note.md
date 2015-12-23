# 参考文献，资料，网站

- 参考游戏 bloxorz 源代码，游戏事件响应和方块渲染都是从这里学来的 (https://hackage.haskell.org/package/bloxorz)
- 大量坑的解决来自于stackoverflow
- 感谢 Google，因为还有的问题来自于奇怪的地方，比如 mailing list，百度当然查不到

# Note

- 运行demo相关
    - 编译依赖
        - GLFW
    - 编译
        - `ghc Main.hs`
    - 运行
        - `./Main`(Linux/OSX), `Main.exe`(Windows)
- 代码架构
    - 外部输入
        - 窗体管理，按键、鼠标点击和移动事件回调，均使用GLFW提供的API
        - GLFW.openWindow, GLFW.closeWindow 用于打开和关闭窗体
        - GLFW.mousePos 用于获取鼠标位置
        - GLFW.getMouseButton, GLFW.getKey 用于获取键盘和鼠标键按下和释放状态
    - 游戏代码执行逻辑
        - loop {
            获取两帧之间的时间差dt
            根据 dt update game state
            {
                获取外部输入
                更新玩家视角，速度
                根据这一帧的玩家速度进行碰撞检测
                更新玩家位置
            }
            根据 update 之后的 game state 渲染屏幕画面
            把新的 game state 作为参数递归调用 loop
          }
    - 值得一提的实现细节
        - "接收外部输入 -> 改变玩家速度 -> 碰撞检测 -> 更新玩家位置" 这样的 update 逻辑使得每个部分逻辑非常模块化，对于增加功能和 debug 都非常有帮助
        - 由于以上设计，update 函数主体部分看起来跟自然描述没有太大区别，很容易加入新的 phase，比如传送门的设定可以直接插入到中间某个部分做一次检查
