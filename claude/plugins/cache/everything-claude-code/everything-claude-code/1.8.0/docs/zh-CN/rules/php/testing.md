---
paths:
  - "**/*.php"
  - "**/phpunit.xml"
  - "**/phpunit.xml.dist"
  - "**/composer.json"
---

# PHP 测试

> 本文档在 [common/testing.md](../common/testing.md) 的基础上，补充了 PHP 相关的内容。

## 测试框架

默认使用 **PHPUnit** 作为测试框架。如果项目已在使用 **Pest**，也是可以接受的。

## 覆盖率

```bash
vendor/bin/phpunit --coverage-text
# or
vendor/bin/pest --coverage
```

在 CI 中优先使用 **pcov** 或 **Xdebug**，并将覆盖率阈值设置在 CI 中，而不是作为团队内部的隐性知识。

## 测试组织

* 将快速的单元测试与涉及框架/数据库的集成测试分开。
* 使用工厂/构建器来生成测试数据，而不是手动编写大量的数组。
* 保持 HTTP/控制器测试专注于传输和验证；将业务规则移到服务层级的测试中。

## 参考

关于整个仓库范围内的 RED -> GREEN -> REFACTOR 循环，请参见技能：`tdd-workflow`。
