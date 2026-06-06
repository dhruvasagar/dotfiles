# 更新日志

## 1.8.0 - 2026-03-04

### 亮点

* 首次发布以可靠性、评估规程和自主循环操作为核心的版本。
* Hook 运行时现在支持基于配置文件的控制和针对性的 Hook 禁用。
* NanoClaw v2 增加了模型路由、技能热加载、分支、搜索、压缩、导出和指标功能。

### 核心

* 新增命令：`/harness-audit`, `/loop-start`, `/loop-status`, `/quality-gate`, `/model-route`。
* 新增技能：
  * `agent-harness-construction`
  * `agentic-engineering`
  * `ralphinho-rfc-pipeline`
  * `ai-first-engineering`
  * `enterprise-agent-ops`
  * `nanoclaw-repl`
  * `continuous-agent-loop`
* 新增代理：
  * `harness-optimizer`
  * `loop-operator`

### Hook 可靠性

* 修复了 SessionStart 的根路径解析，增加了健壮的回退搜索。
* 将会话摘要持久化移至 `Stop`，此处可获得转录负载。
* 增加了质量门和成本追踪钩子。
* 用专门的脚本文件替换了脆弱的单行内联钩子。
* 增加了 `ECC_HOOK_PROFILE` 和 `ECC_DISABLED_HOOKS` 控制。

### 跨平台

* 改进了文档警告逻辑中 Windows 安全路径的处理。
* 强化了观察者循环行为，以避免非交互式挂起。

### 备注

* `autonomous-loops` 作为一个兼容性别名保留一个版本；`continuous-agent-loop` 是规范名称。

### 鸣谢

* 灵感来自 [zarazhangrui](https://github.com/zarazhangrui)
* homunculus 灵感来自 [humanplane](https://github.com/humanplane)
