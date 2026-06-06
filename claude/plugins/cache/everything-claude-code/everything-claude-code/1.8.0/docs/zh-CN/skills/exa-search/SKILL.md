---
name: exa-search
description: 通过Exa MCP进行神经搜索，适用于网络、代码和公司研究。当用户需要网络搜索、代码示例、公司情报、人员查找，或使用Exa神经搜索引擎进行AI驱动的深度研究时使用。
origin: ECC
---

# Exa 搜索

通过 Exa MCP 服务器实现网页内容、代码、公司和人物的神经搜索。

## 何时激活

* 用户需要当前网页信息或新闻
* 搜索代码示例、API 文档或技术参考资料
* 研究公司、竞争对手或市场参与者
* 查找特定领域的专业资料或人物
* 为任何开发任务进行背景调研
* 用户提到“搜索”、“查找”、“寻找”或“关于……的最新消息是什么”

## MCP 要求

必须配置 Exa MCP 服务器。添加到 `~/.claude.json`：

```json
"exa-web-search": {
  "command": "npx",
  "args": [
    "-y",
    "exa-mcp-server",
    "tools=web_search_exa,web_search_advanced_exa,get_code_context_exa,crawling_exa,company_research_exa,people_search_exa,deep_researcher_start,deep_researcher_check"
  ],
  "env": { "EXA_API_KEY": "YOUR_EXA_API_KEY_HERE" }
}
```

在 [exa.ai](https://exa.ai) 获取 API 密钥。
如果省略 `tools=...` 参数，可能只会启用较小的默认工具集。

## 核心工具

### web\_search\_exa

用于当前信息、新闻或事实的通用网页搜索。

```
web_search_exa(query: "latest AI developments 2026", numResults: 5)
```

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|-------|------|---------|-------|
| `query` | string | 必需 | 搜索查询 |
| `numResults` | number | 8 | 结果数量 |

### web\_search\_advanced\_exa

具有域名和日期约束的过滤搜索。

```
web_search_advanced_exa(
  query: "React Server Components best practices",
  numResults: 5,
  includeDomains: ["github.com", "react.dev"],
  startPublishedDate: "2025-01-01"
)
```

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|-------|------|---------|-------|
| `query` | string | 必需 | 搜索查询 |
| `numResults` | number | 8 | 结果数量 |
| `includeDomains` | string\[] | 无 | 限制在特定域名 |
| `excludeDomains` | string\[] | 无 | 排除特定域名 |
| `startPublishedDate` | string | 无 | ISO 日期过滤器（开始） |
| `endPublishedDate` | string | 无 | ISO 日期过滤器（结束） |

### get\_code\_context\_exa

从 GitHub、Stack Overflow 和文档站点查找代码示例和文档。

```
get_code_context_exa(query: "Python asyncio patterns", tokensNum: 3000)
```

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|-------|------|---------|-------|
| `query` | string | 必需 | 代码或 API 搜索查询 |
| `tokensNum` | number | 5000 | 内容令牌数（1000-50000） |

### company\_research\_exa

用于商业情报和新闻的公司研究。

```
company_research_exa(companyName: "Anthropic", numResults: 5)
```

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|-------|------|---------|-------|
| `companyName` | string | 必需 | 公司名称 |
| `numResults` | number | 5 | 结果数量 |

### people\_search\_exa

查找专业资料和个人简介。

```
people_search_exa(query: "AI safety researchers at Anthropic", numResults: 5)
```

### crawling\_exa

从 URL 提取完整页面内容。

```
crawling_exa(url: "https://example.com/article", tokensNum: 5000)
```

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|-------|------|---------|-------|
| `url` | string | 必需 | 要提取的 URL |
| `tokensNum` | number | 5000 | 内容令牌数 |

### deep\_researcher\_start / deep\_researcher\_check

启动一个异步运行的 AI 研究代理。

```
# Start research
deep_researcher_start(query: "comprehensive analysis of AI code editors in 2026")

# Check status (returns results when complete)
deep_researcher_check(researchId: "<id from start>")
```

## 使用模式

### 快速查找

```
web_search_exa(query: "Node.js 22 new features", numResults: 3)
```

### 代码研究

```
get_code_context_exa(query: "Rust error handling patterns Result type", tokensNum: 3000)
```

### 公司尽职调查

```
company_research_exa(companyName: "Vercel", numResults: 5)
web_search_advanced_exa(query: "Vercel funding valuation 2026", numResults: 3)
```

### 技术深度研究

```
# Start async research
deep_researcher_start(query: "WebAssembly component model status and adoption")
# ... do other work ...
deep_researcher_check(researchId: "<id>")
```

## 提示

* 使用 `web_search_exa` 进行广泛查询，使用 `web_search_advanced_exa` 获取过滤结果
* 较低的 `tokensNum`（1000-2000）用于聚焦的代码片段，较高的（5000+）用于全面的上下文
* 结合 `company_research_exa` 和 `web_search_advanced_exa` 进行彻底的公司分析
* 使用 `crawling_exa` 从搜索结果中的特定 URL 获取完整内容
* `deep_researcher_start` 最适合受益于 AI 综合的全面主题

## 相关技能

* `deep-research` — 使用 firecrawl + exa 的完整研究工作流
* `market-research` — 带有决策框架的业务导向研究
