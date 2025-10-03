;;; setup-development.el --- 開発環境設定 -*- lexical-binding: t -*-

;;; Commentary:
;; LSP、補完、構文チェックなどの開発環境設定

;;; Code:

;; Company - 補完システム（遅延読み込み）
(use-package company
  :straight t
  :defer 2
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-show-quick-access t))

;; LSP Mode
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-file-watch-threshold 2000
        lsp-enable-file-watchers nil
        lsp-eldoc-enable-hover t
        lsp-signature-auto-activate t))

;; LSP UI
(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t))

;; LSP Ivy
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

;; Flycheck - 構文チェック（遅延読み込み）
(use-package flycheck
  :straight t
  :defer 3
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Yasnippet - スニペット
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t)

;; Rainbow delimiters - 括弧の色分け
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Multiple cursors
(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Format All
(use-package format-all
  :straight t
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Python" black)
                  ("JavaScript" prettier)
                  ("TypeScript" prettier)
                  ("JSON" prettier)
                  ("CSS" prettier)
                  ("HTML" prettier))))

;; 言語固有設定

;; Python
(use-package python-mode
  :straight t
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))

;; JavaScript/TypeScript
(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "multimarkdown"))

;; YAML
(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")

;; JSON
(use-package json-mode
  :straight t
  :mode "\\.json\\'")

;; Ruby/Rails development
(use-package ruby-mode
  :straight nil  ; built-in package
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (setq ruby-indent-level 2
        ruby-deep-indent-paren nil))

;; Robe - Ruby completion and navigation
(use-package robe
  :straight t
  :hook (ruby-mode . robe-mode)
  :config
  ;; Company integration
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-robe))
  ;; Start robe server automatically in Rails projects
  (defun auto-start-robe-in-rails ()
    (when (and (buffer-file-name)
               (string-match "app\\|lib\\|config\\|spec\\|test" (buffer-file-name))
               (locate-dominating-file default-directory "Gemfile"))
      (robe-start)))
  (add-hook 'ruby-mode-hook 'auto-start-robe-in-rails))

;; Projectile Rails
(use-package projectile-rails
  :straight t
  :hook (ruby-mode . projectile-rails-mode)
  :config
  (setq projectile-rails-custom-server-command "rails server"))

;; Inf-ruby for REPL
(use-package inf-ruby
  :straight t
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding"))

;; RSpec support
(use-package rspec-mode
  :straight t
  :hook (ruby-mode . rspec-mode)
  :config
  (setq rspec-use-spring-when-possible t)
  (setq rspec-use-bundler-when-possible t))

; YAML mode already defined above

;; Enhanced Ruby support
(use-package ruby-electric
  :straight t
  :hook (ruby-mode . ruby-electric-mode))

;; Better Ruby end handling
(use-package ruby-end
  :straight t
  :hook (ruby-mode . ruby-end-mode))

(provide 'setup-development)
;;; setup-development.el ends here