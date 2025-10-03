;;; setup-ui.el --- UI とテーマ設定 -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs の見た目とユーザーインターフェース設定

;;; Code:

;; テーマ設定（ターミナル専用） - 背景制御をtmuxに委譲
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t)
  ;; 背景を透明にしてtmuxの色制御に任せる
  (set-face-background 'default "unspecified-bg")
  (set-face-background 'fringe "unspecified-bg"))

;; Mode line の改善（ターミナル専用設定）
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-minor-modes t          ; マイナーモード表示
        doom-modeline-buffer-encoding t      ; エンコーディング表示  
        doom-modeline-checker-simple-format nil  ; 詳細なチェッカー情報
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t          ; 言語バージョン表示
        doom-modeline-time nil               ; 重複回避
        doom-modeline-workspace-name nil     ; シンプルに
        doom-modeline-lsp t                  ; LSP状態表示
        doom-modeline-github nil))           ; 無効化

;; カスタム情報をモードラインに追加（適度ににぎやか）
(defun custom-modeline-info ()
  "カスタムモードライン情報"
  (concat
   " [" (format-time-string "%H:%M") "]"
   (when (buffer-file-name)
     (concat " [" (file-name-extension (buffer-file-name) t) "]"))
   " [L:" (number-to-string (count-lines (point-min) (point-max))) "]"
   " [C:" (number-to-string (current-column)) "]"))

(setq-default mode-line-format 
  (append mode-line-format '((:eval (custom-modeline-info)))))


;; ウィンドウ分割の改善
(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Dashboard（ターミナル版）
(use-package dashboard
  :straight t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner 2
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                         (projects . 5))
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-set-navigator t
        dashboard-navigator-buttons
        '(("[G]" "GitHub" "Browse GitHub"
           (lambda (&rest _) (browse-url "https://github.com")))
          ("[C]" "Claude Code" "Claude Code integration"
           (lambda (&rest _) (message "Claude Code integration active"))))))

;; Highlight current line
(global-hl-line-mode 1)

;; Show column numbers
(column-number-mode 1)

;; テスト：カーソル行のハイライト色を変更
(set-face-background 'hl-line "#4a4a4a")

;; 括弧のハイライト改善
(use-package highlight-parentheses
  :straight t
  :hook (prog-mode . highlight-parentheses-mode))

;; Rainbow delimiters - 括弧の階層を色分け
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; シンボルハイライト - カーソル下のシンボルを強調
(use-package highlight-symbol
  :straight t
  :bind (("C-c h h" . highlight-symbol)
         ("C-c h n" . highlight-symbol-next)
         ("C-c h p" . highlight-symbol-prev)
         ("C-c h r" . highlight-symbol-remove-all))
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5))

;; Beacon - カーソル位置を一時的に強調
(use-package beacon
  :straight t
  :config
  (beacon-mode 1)
  (setq beacon-color "#ff6c6b"
        beacon-size 20
        beacon-blink-delay 0.3))

;; インデントガイド
(use-package highlight-indent-guides
  :straight t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-enabled nil)  ; 自動フェイス設定を無効化
  ;; カスタムカラー設定
  (set-face-foreground 'highlight-indent-guides-character-face "#4a4a4a")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#ff6c6b"))

;; Smooth scrolling
(use-package smooth-scrolling
  :straight t
  :config
  (smooth-scrolling-mode 1))

;; ターミナルでのマウススクロール設定
(when (not window-system)
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line))

;; Emacsの背景制御を無効化 - tmuxに任せる
;; (フォーカス関連の機能をすべて削除)
;; tmuxのpane-active-borderとpane-borderがフォーカス制御を担当

;; ウィンドウサイズ調整
(defun adjust-window-size (direction)
  "ウィンドウサイズを調整"
  (interactive)
  (cond
   ((eq direction 'up) (enlarge-window -1))
   ((eq direction 'down) (enlarge-window 1))
   ((eq direction 'left) (enlarge-window-horizontally -1))
   ((eq direction 'right) (enlarge-window-horizontally 1))))

(global-set-key (kbd "C-c <up>") (lambda () (interactive) (adjust-window-size 'up)))
(global-set-key (kbd "C-c <down>") (lambda () (interactive) (adjust-window-size 'down)))
(global-set-key (kbd "C-c <left>") (lambda () (interactive) (adjust-window-size 'left)))
(global-set-key (kbd "C-c <right>") (lambda () (interactive) (adjust-window-size 'right)))

(provide 'setup-ui)
;;; setup-ui.el ends here
