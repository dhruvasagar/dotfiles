---
description: 从 ~/.claude/sessions/ 加载最新的会话文件，并从上次会话结束的地方恢复工作，保留完整上下文。
---

# 恢复会话命令

加载最后保存的会话状态，并在开始任何工作前完全熟悉情况。
此命令是 `/save-session` 的对应命令。

## 何时使用

* 开始新会话以继续前一天的工作时
* 因上下文限制而开始全新会话后
* 当从其他来源移交会话文件时（只需提供文件路径）
* 任何拥有会话文件并希望 Claude 在继续前完全吸收其内容的时候

## 用法

```
/resume-session                                                      # loads most recent file in ~/.claude/sessions/
/resume-session 2024-01-15                                           # loads most recent session for that date
/resume-session ~/.claude/sessions/2024-01-15-session.tmp           # loads a specific legacy-format file
/resume-session ~/.claude/sessions/2024-01-15-abc123de-session.tmp  # loads a current short-id session file
```

## 流程

### 步骤 1：查找会话文件

如果未提供参数：

1. 检查 `~/.claude/sessions/`
2. 选择最近修改的 `*-session.tmp` 文件
3. 如果文件夹不存在或没有匹配的文件，告知用户：
   ```
   在 ~/.claude/sessions/ 中未找到会话文件。
   请在会话结束时运行 /save-session 来创建一个。
   ```
   然后停止。

如果提供了参数：

* 如果看起来像日期 (`YYYY-MM-DD`)，则在 `~/.claude/sessions/` 中搜索匹配
  `YYYY-MM-DD-session.tmp`（旧格式）或 `YYYY-MM-DD-<shortid>-session.tmp`（当前格式）的文件，
  并加载该日期最近修改的版本
* 如果看起来像文件路径，则直接读取该文件
* 如果未找到，清晰报告并停止

### 步骤 2：读取整个会话文件

读取完整的文件。暂时不要总结。

### 步骤 3：确认理解

使用以下确切格式回复一份结构化简报：

```
SESSION LOADED: [actual resolved path to the file]
════════════════════════════════════════════════

PROJECT: [project name / topic from file]

WHAT WE'RE BUILDING:
[2-3 sentence summary in your own words]

CURRENT STATE:
✅ Working: [count] items confirmed
🔄 In Progress: [list files that are in progress]
🗒️ Not Started: [list planned but untouched]

WHAT NOT TO RETRY:
[list every failed approach with its reason — this is critical]

OPEN QUESTIONS / BLOCKERS:
[list any blockers or unanswered questions]

NEXT STEP:
[exact next step if defined in the file]
[if not defined: "No next step defined — recommend reviewing 'What Has NOT Been Tried Yet' together before starting"]

════════════════════════════════════════════════
Ready to continue. What would you like to do?
```

### 步骤 4：等待用户

请**不要**自动开始工作。请**不要**触碰任何文件。等待用户指示下一步做什么。

如果会话文件中明确定义了下一步，并且用户说"继续"或"是"或类似内容 — 则执行该确切步骤。

如果未定义下一步 — 询问用户从哪里开始，并可选择性地从"尚未尝试的内容"部分提出建议。

***

## 边界情况

**同一日期有多个会话** (`2024-01-15-session.tmp`, `2024-01-15-abc123de-session.tmp`)：
加载该日期最近修改的匹配文件，无论其使用的是旧的无ID格式还是当前的短ID格式。

**会话文件引用了已不存在的文件：**
在简报中注明 — "⚠️ 会话中引用了 `path/to/file.ts`，但在磁盘上未找到。"

**会话文件来自超过7天前：**
注明时间间隔 — "⚠️ 此会话来自 N 天前（阈值：7天）。情况可能已发生变化。" — 然后正常继续。

**用户直接提供了文件路径（例如，从队友处转发而来）：**
读取它并遵循相同的简报流程 — 无论来源如何，格式都是相同的。

**会话文件为空或格式错误：**
报告："找到会话文件，但似乎为空或无法读取。您可能需要使用 /save-session 创建一个新的。"

***

## 示例输出

```
SESSION LOADED: /Users/you/.claude/sessions/2024-01-15-abc123de-session.tmp
════════════════════════════════════════════════

PROJECT: my-app — JWT Authentication

WHAT WE'RE BUILDING:
User authentication with JWT tokens stored in httpOnly cookies.
Register and login endpoints are partially done. Route protection
via middleware hasn't been started yet.

CURRENT STATE:
✅ Working: 3 items (register endpoint, JWT generation, password hashing)
🔄 In Progress: app/api/auth/login/route.ts (token works, cookie not set yet)
🗒️ Not Started: middleware.ts, app/login/page.tsx

WHAT NOT TO RETRY:
❌ Next-Auth — conflicts with custom Prisma adapter, threw adapter error on every request
❌ localStorage for JWT — causes SSR hydration mismatch, incompatible with Next.js

OPEN QUESTIONS / BLOCKERS:
- Does cookies().set() work inside a Route Handler or only Server Actions?

NEXT STEP:
In app/api/auth/login/route.ts — set the JWT as an httpOnly cookie using
cookies().set('token', jwt, { httpOnly: true, secure: true, sameSite: 'strict' })
then test with Postman for a Set-Cookie header in the response.

════════════════════════════════════════════════
Ready to continue. What would you like to do?
```

***

## 注意事项

* 加载时切勿修改会话文件 — 它是一个只读的历史记录
* 简报格式是固定的 — 即使某些部分为空，也不要跳过
* "不应重试的内容"必须始终显示，即使它只是说"无" — 这太重要了，不容遗漏
* 恢复后，用户可能希望在新的会话结束时再次运行 `/save-session`，以创建一个新的带日期文件
