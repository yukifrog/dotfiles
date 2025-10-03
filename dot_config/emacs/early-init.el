;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs 27以降で最初に実行される初期化ファイル
;; パッケージシステムの設定やUI要素の早期設定を行う

;;; Code:

;; XDG Base Directory に対応
(setq user-emacs-directory (expand-file-name "emacs/" (or (getenv "XDG_CONFIG_HOME") "~/.config/")))

;; パッケージシステムの初期化を遅延
(setq package-enable-at-startup nil)

;; 起動高速化設定
(setq gc-cons-threshold most-positive-fixnum) ; 起動時GCを最小化
(setq gc-cons-percentage 0.6)

;; UI要素を早期に無効化（GUI使用時）
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; フレーム設定の最適化
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here