;;; setup-project.el --- プロジェクト管理とファイル操作設定 -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code との連携を重視したプロジェクト管理設定

;;; Code:

;; Projectile - プロジェクト管理
(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf))

;; Magit - Git統合
(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-auto-revert-mode t))

;; Git Gutter - 変更箇所の視覚化
(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Dired改善
(use-package dired
  :straight nil  ; built-in package
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top))

;; Treemacs - ファイルツリー
(use-package treemacs
  :straight t
  :bind ("C-x t t" . treemacs)
  :config
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;; Which-key - キーバインドヘルプ
(use-package which-key
  :straight t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

;; Ivy/Counsel - 検索とナビゲーション
(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git-grep))
  :config
  (counsel-mode 1))

(use-package swiper
  :straight t
  :bind ("C-s" . swiper))

(provide 'setup-project)
;;; setup-project.el ends here