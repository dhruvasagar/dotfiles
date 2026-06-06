---
name: setup-semgrep-plugin
description: Set up the Semgrep plugin by installing Semgrep, authenticating, and verifying compatibility
---
# Setup Semgrep Plugin

Follow these steps to set up the Semgrep plugin:

## 1. Install Semgrep

Check if Semgrep is installed, and install it if not:

```bash
which semgrep || brew install semgrep
```

## 2. Authenticate with Semgrep

Log in to Semgrep (this will open a browser window):

```bash
semgrep login --force
```

## 3. Install Semgrep Pro Engine

Install the Pro engine for enhanced scanning capabilities:

```bash
semgrep install-semgrep-pro || true
```

## 4. Verify Installation

Confirm everything is working:

```bash
semgrep --pro --version
```

## 5. Check Version Compatibility

Verify your Semgrep version is >= 1.146.0:

```bash
semgrep --version
```

If your version is older than 1.146.0, please update:
```bash
brew upgrade semgrep
```
