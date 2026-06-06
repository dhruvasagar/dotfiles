# 工具链审计命令

审计当前代码库的智能体工具链设置并返回一份优先级评分卡。

## 使用方式

`/harness-audit [scope] [--format text|json]`

* `scope` (可选): `repo` (默认), `hooks`, `skills`, `commands`, `agents`
* `--format`: 输出样式 (`text` 默认, `json` 用于自动化)

## 评估内容

将每个类别从 `0` 到 `10` 进行评分：

1. 工具覆盖度
2. 上下文效率
3. 质量门禁
4. 记忆持久化
5. 评估覆盖度
6. 安全护栏
7. 成本效率

## 输出约定

返回：

1. `overall_score` (满分 70)
2. 类别得分和具体发现
3. 前 3 项待办事项及其确切文件路径
4. 建议接下来应用的 ECC 技能

## 检查清单

* 检查 `hooks/hooks.json`, `scripts/hooks/` 以及钩子测试。
* 检查 `skills/`、命令覆盖度和智能体覆盖度。
* 验证 `.cursor/`, `.opencode/`, `.codex/` 在跨工具链间的一致性。
* 标记已损坏或过时的引用。

## 结果示例

```text
Harness Audit (repo): 52/70
- Quality Gates: 9/10
- Eval Coverage: 6/10
- Cost Efficiency: 4/10

Top 3 Actions:
1) Add cost tracking hook in scripts/hooks/cost-tracker.js
2) Add pass@k docs and templates in skills/eval-harness/SKILL.md
3) Add command parity for /harness-audit in .opencode/commands/
```

## 参数

$ARGUMENTS:

* `repo|hooks|skills|commands|agents` (可选范围)
* `--format text|json` (可选输出格式)
