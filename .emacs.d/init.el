(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next)
;(define-key company-active-map (kbd "C-p") 'company-select-previous)
;(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
;(define-key company-search-map (kbd "C-n") 'company-select-next)
;(define-key company-search-map (kbd "C-p") 'company-select-previous)
;;  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(setq company-minimum-prefix-length 2)
(setq company-search-filtering t)
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)

;;whitespace-mode
;;https://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'whitespace-action) nil)))
;(global-whitespace-mode 1)






(straight-use-package 'flycheck)
(global-flycheck-mode)
;;rubocop flycheck lint
;;gem install rubocop ruby-lint pry pry-dock reek
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq flycheck-checker 'ruby-rubocop)
	     (flycheck-mode 1)))
;; 自動起動
(setq flycheck-check-syntax-automatically
  '(save idle-change mode-enabled))

;; コード変更後、3秒後にチェックする
(setq flycheck-idle-change-delay 3)
;;(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
;;flyspell?
;https://www.m3tech.blog/entry/emacs-web-service
;flymake
;;(straight-use-package 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
;;projectile








;;hl-line-mode
;;https://www.emacswiki.org/emacs/HighlightCurrentLine
;;(global-hl-line-mode -1)

;;https://github.com/mahito1594/dotemacs
(straight-use-package 'beacon)
(setq beacon-blink-duration 1)
(setq beacon-color "white")
(setq beacon-dont-blink-commands nil)
(add-hook 'after-init-hook 'beacon-mode)

;;https://github.com/k-talo/volatile-highlights.el
(straight-use-package 'volatile-highlights)
(volatile-highlights-mode t)

;;https://github.com/DarthFennec/highlight-indent-guides
(straight-use-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-auto-enabled t)
(setq highlight-indent-guides-responsive t)
(setq highlight-indent-guides-method 'character)


(straight-use-package 'robe)
;; M-x inf-ruby
;; M-x robe-start
;; 自動で出来るかな



(setq ruby-deep-indent-paren nil)

;;(global-set-key (kbd "C-c r r") 'inf-ruby)

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; helm https://tuhdo.github.io/helm-intro.html
(straight-use-package 'helm)
;;helm helm-config?
;(straight-use-package 'consel)
;;ivy-rich
;; https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf


;; ace-window
;; https://github.com/abo-abo/ace-window


;;lsp-mode?
;;https://github.com/emacs-lsp/lsp-mode


(straight-use-package 'ivy)
(straight-use-package 'swiper)
(global-set-key (kbd "M-x") #'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(helm-mode 1)
(global-set-key (kbd "C-s") 'swiper)

(straight-use-package 'ruby-block)
(require 'ruby-block) ;; <- なぜか必要
(ruby-block-mode t)
(straight-use-package 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(straight-use-package 'pry)
(straight-use-package 'inf-ruby)


;;https://github.com/TeMPOraL/nyan-mode
(straight-use-package 'nyan-mode)
(nyan-mode t)

;;rbenv
;;ruby end <- ruby-electricとかぶってる
;;projectile?


;;smart-newline.el https://github.com/ainame/smart-newline.el
(straight-use-package 'smart-newline)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (smart-newline-mode 1)))
;;dump-jump
;;flyspell
;;rinari
;tag?
;;migemo
;;magit
(straight-use-package 'forge)
(straight-use-package 'magit)

(straight-use-package 'yasnippet)
(yas-global-mode t)
(straight-use-package 'yasnippet-snippets)


;;git-company

;;neotree
;;https://github.com/jaypei/emacs-neotree
(straight-use-package 'neotree)

;;imenu
(straight-use-package 'imenu-list)


;;minimap


;;hide-mode-line
;;amex semex
;;hydra https://github.com/abo-abo/hydra

(straight-use-package 'yaml-mode)


;show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;括弧の自動挿入
(electric-pair-mode 1)

(tool-bar-mode -1)

;https://emacs-jp.github.io/packages/anzu
(straight-use-package 'anzu)
(straight-use-package 'migemo)

(global-anzu-mode t)
(setq anzu-search-threshold 1000)
;; migemoを利用している場合
;(setq anzu-use-migemo t)


;skk
;https://ddskk.readthedocs.io/ja/latest/
(straight-use-package 'ddskk)
(global-set-key (kbd "C-x C-j") 'skk-mode)
;(global-set-key (kbd "<zenkaku-hankaku>") 'skk-mode)
(global-set-key (kbd "<zenkaku-hankaku>") 'skk-auto-fill-mode)
(setq skk-show-mode-show t)
(setq skk-show-mode-style 'tooltip)
;C-h v 変数の中身を確認
;backspase C-h

;;which-key
;;http://emacs.rubikitch.com/which-key/
(straight-use-package 'which-key)
;;; 3つの表示方法どれか1つ選ぶ
(which-key-setup-side-window-bottom)    ;ミニバッファ
;; (which-key-setup-side-window-right)     ;右端
;; (which-key-setup-side-window-right-bottom) ;両方使う
(which-key-mode 1)

(straight-use-package 'rainbow-delimiters)
;(rainbow-delimiters-mode-enable)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable)

(provide 'init)
;;; init.el ends here
