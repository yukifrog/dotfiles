# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Emacs Configuration Architecture

This is a comprehensive Emacs configuration optimized for Claude Code integration. The configuration uses symbolic links from `~/.emacs.d` to this `dotemacs` directory for easy version control.

### Configuration Structure

```
dotemacs/
├── init.el                 # Main configuration entry point
├── lisp/                   # Modular configuration files
│   ├── setup-project.el    # Project management (Projectile, Magit, Treemacs)
│   ├── setup-development.el # Development tools (LSP, Company, Flycheck)
│   ├── setup-claude-code.el # Claude Code integration functions
│   └── setup-ui.el         # UI and themes (Doom themes/modeline)
├── backups/                # Automatic backups directory
├── auto-save/              # Auto-save files directory
├── snippets/               # Custom snippets
└── CLAUDE.md               # This file
```

### Key Features for Claude Code Integration

**Custom Functions** (all bound to `C-c C-*` keys):
- `claude-code-copy-file-path`: Copy current file path
- `claude-code-copy-file-with-line`: Copy file:line format
- `claude-code-copy-relative-path`: Copy project-relative path
- `claude-code-copy-function-context`: Copy function with context
- `claude-code-create-scratch-buffer`: Create work buffer
- `claude-code-show-project-structure`: Display project tree

**Development Stack**:
- **Project Management**: Projectile + Treemacs + Magit
- **Code Intelligence**: LSP-mode with UI enhancements
- **Completion**: Company-mode with quick access
- **Search/Navigation**: Ivy/Counsel/Swiper
- **Syntax**: Flycheck + format-all + yasnippet
- **UI**: Doom themes, modeline, dashboard

### Architecture Principles

- **Modular Design**: Configuration split into logical modules in `lisp/`
- **Self-bootstrapping**: Automatic package installation on first run
- **Performance Optimized**: Deferred loading and caching enabled
- **Claude Code First**: Custom functions for seamless AI-assisted development

### Setup Commands

No build commands needed - configuration is self-managing:
- First run automatically installs all packages
- Language servers need manual installation (npm, pip, apt)
- All settings take effect immediately

### Working with This Configuration

- Edit configuration files in `dotemacs/` - changes are immediately active
- Use `C-x C-e` to evaluate elisp expressions
- Restart Emacs if major changes don't take effect
- All Claude Code integration functions use `C-c C-` prefix