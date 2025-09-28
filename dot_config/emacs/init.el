;;; init.el --- Claude Code + Emacs 統合環境設定 -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code との連携を重視した Emacs 設定
;; dotfiles として管理し、シンボリックリンクで使用

;;; Code:

;; 起動時間測定
(defvar emacs-start-time (current-time))
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs loaded in %s"
      (format "%.2f seconds"
        (float-time (time-subtract (current-time) emacs-start-time))))))

;; 起動高速化設定
(setq gc-cons-threshold most-positive-fixnum) ; 起動時GCを最小化
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ; LSPパフォーマンス向上

;; 起動後にGC設定を戻す
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216) ; 16mb
    (setq gc-cons-percentage 0.1)
    (garbage-collect)))

;; straight.elのみを使用するため、package.elの設定は削除

;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageはstraight.el経由でインストール

;; straight.el integration with use-package
(straight-use-package 'use-package)

(require 'use-package)
(setq use-package-always-ensure nil  ; straight.elを使うのでnilに設定
      use-package-verbose t)

;; 基本的なUI設定
(setq inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function 'ignore)

;; GUI要素の無効化（emacs-nox対応）
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; 基本機能の遅延有効化
(add-hook 'emacs-startup-hook
  (lambda ()
    (global-display-line-numbers-mode 1)
    (electric-pair-mode 1)
    (show-paren-mode 1)
    (global-auto-revert-mode 1)))

;; ファイル管理
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; 追加設定ファイルの読み込み
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; カスタムファイル
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 設定ファイルの自動リロード（遅延読み込み）
(defun reload-emacs-config ()
  "Emacs設定ファイルを自動リロード"
  (interactive)
  (dolist (file '("setup-project" "setup-development" "setup-claude-code-ide" "setup-rails" "setup-ui"))
    (load file))
  (message "Emacs configuration reloaded!"))

;; .config/emacs/lisp内のファイル保存時に自動リロード（遅延起動）
(run-with-idle-timer 2 nil
  (lambda ()
    (add-hook 'after-save-hook
      (lambda ()
        (when (and (buffer-file-name)
                   (string-match-p "/\\.config/emacs/lisp/" (buffer-file-name)))
          (run-with-timer 0.5 nil 'reload-emacs-config))))))

;; プロジェクト管理とファイル操作
(load "setup-project")

;; 開発環境
(load "setup-development")

;; Claude Code IDE連携 (MCP統合版)
(load "setup-claude-code-ide")

;; Rails開発設定
(load "setup-rails")

;; テーマとUI
(load "setup-ui")

;;; init.el ends here