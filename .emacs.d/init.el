;;https://github.com/raxod502/straight.el
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










;; counsel swiper ivy
;; https://github.com/abo-abo/swiper
(straight-use-package 'counsel)
(straight-use-package 'ivy)
(straight-use-package 'swiper)

(setq ivy-use-virtual-buffers t)

;;(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-s") 'swiper)
(straight-use-package 'lsp-ivy)
;;ivy-rich

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-y") 'counsel-yank-pop)
(setq counsel-yank-pop-preselect-last 0)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-M-z") 'counsel-fzf)
(global-set-key (kbd "C-M-r") 'counsel-recentf)
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-M-f") 'counsel-ag)
(counsel-mode 1)

;;amex semex
(straight-use-package 'amx)
(setq amx-backend 'ivy)




;; lsp

;;https://github.com/emacs-lsp/lsp-mode
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;(setq lsp-keymap-prefix "C-l")

(straight-use-package 'lsp-mode)
(add-hook 'ruby-mode-hook #'lsp)
;(straight-use-package 'lsp-ruby)
;(straight-use-package 'lsp-solargraph)
;(require 'solargraph)
;(define-key ruby-mode-map (kbd "M-i") 'solargraph:complete)

;;https://emacs-lsp.github.io/lsp-ui/
(straight-use-package 'lsp-ui)
;lsp-ui-doc

;;(straight-use-package 'helm-lsp)

;(straight-use-package 'lsp-treemacs)
(straight-use-package 'dap-mode)
;(straight-use-package 'dap-ruby)

;(eval-after-load 'company
;  '(push 'company-lsp company-backends))


(add-hook 'ruby-mode-hook #'lsp)


;;http://company-mode.github.io/
(straight-use-package 'company)
(straight-use-package 'company-statistics)
(add-hook 'after-init-hook
	  (lambda()
	    (global-company-mode)
	    (company-statistics-mode)
	    (define-key company-active-map (kbd "C-n") 'company-select-next)
	    (define-key company-active-map (kbd "C-p") 'company-select-previous)
	    (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
	    (define-key company-search-map (kbd "C-n") 'company-select-next)
	    (define-key company-search-map (kbd "C-p") 'company-select-previous)
	    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
	    (setq company-minimum-prefix-length 1)
	    (setq company-search-filtering t)
	    (setq company-selection-wrap-around nil) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
	    (setq completion-ignore-case t)
	    (setq company-dabbrev-downcase nil)
	    (setq company-idle-delay 0.1)
	    (add-to-list 'company-backends '(company-capf :with company-dabbrev))
	    (add-to-list 'company-backends '(company-capf :with company-yasnippet))
	    )
	  )

;;company-box
;;company-quickhelp


;;whitespace-mode
;;https://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
;;https://w.atwiki.jp/ntemacs/pages/76.html
(require 'whitespace)
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'whitespace-action) nil)))
(global-whitespace-mode -1)
(set-face-attribute 'whitespace-space nil
                    :background "#232323"
                    :foreground "GreenYellow"
                    :weight 'bold)




;;https://github.com/flycheck/flycheck
(straight-use-package 'flycheck)
;(global-flycheck-mode)
;;rubocop flycheck lint
;;gem install rubocop ruby-lint pry pry-dock reek
;(add-hook 'ruby-mode-hook
;	  '(lambda ()
;	     (setq flycheck-checker 'ruby-rubocop)
;	     (flycheck-mode 1)))

;; 自動起動
(setq flycheck-check-syntax-automatically
  '(save idle-change mode-enabled))

;; コード変更後、x秒後にチェックする
(setq flycheck-idle-change-delay 2)
;;(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
;;flyspell?
;https://www.m3tech.blog/entry/emacs-web-service
;flymake
;;(straight-use-package 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;;https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
(straight-use-package 'projectile)
(projectile-mode +1)
(straight-use-package 'projectile-rails)
(projectile-rails-global-mode)
;(add-hook 'projectile-mode-hook 'projectile-rails-on)



;;hl-line-mode
;;https://www.emacswiki.org/emacs/HighlightCurrentLine
;;(global-hl-line-mode -1)

;;https://github.com/mahito1594/dotemacs
(straight-use-package 'beacon)
(setq beacon-blink-duration 1)
(setq beacon-color "black")
(setq beacon-dont-blink-commands nil)
(add-hook 'after-init-hook 'beacon-mode)

;;https://github.com/k-talo/volatile-highlights.el
;(straight-use-package 'volatile-highlights)
;(volatile-highlights-mode t)

;;https://github.com/DarthFennec/highlight-indent-guides
;(straight-use-package 'highlight-indent-guides)
;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;(setq highlight-indent-guides-auto-enabled t)
;(setq highlight-indent-guides-responsive t)
;(setq highlight-indent-guides-method 'character)


;;(straight-use-package 'robe)
;;(straight-use-package 'helm-robe)
;; M-x inf-ruby
;; M-x robe-start
;; 自動で出来るかな


;;https://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
(setq ruby-deep-indent-paren nil)


;;(add-hook 'ruby-mode-hook 'robe-mode)
;;(eval-after-load 'company
;;  '(push 'company-robe company-backends))

;; helm https://tuhdo.github.io/helm-intro.html
(straight-use-package 'helm)
(helm-mode 1)
;;helm helm-config?

;; https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf


;; ace-window
;; https://github.com/abo-abo/ace-window

;;avy

;;lsp-mode?
;;https://github.com/emacs-lsp/lsp-mode



;;(global-set-key (kbd "M-x") #'helm-M-x)

;(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
;(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;(global-set-key (kbd "C-x b") 'helm-mini)
;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;(global-set-key (kbd "C-c t") 'helm-recentf)
;(setq helm-buffers-fuzzy-matching t
;      helm-recentf-fuzzy-match    t)



(recentf-mode 1)



(straight-use-package 'ruby-block)
(require 'ruby-block) ;; <- なぜか必要
(ruby-block-mode t)
(straight-use-package 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(straight-use-package 'pry)
(straight-use-package 'inf-ruby)
(straight-use-package 'rbenv)
(global-rbenv-mode)
;;ruby end <- ruby-electricとかぶってる
;;inf-ruby
;;rubocop
;;robe
;;bundler
;;rake
;;rvm
;;chruby
;;rspec-mode
;;minitest





;;https://github.com/TeMPOraL/nyan-mode
(straight-use-package 'nyan-mode)
(nyan-mode t)




;;projectile?
;;yard-mode?
;;projectile-rails
;;inflections


;;smart-newline.el https://github.com/ainame/smart-newline.el
;;C-m or RET
;;改行したときにいんでんとした場所にかーそるがいく
(straight-use-package 'smart-newline)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (smart-newline-mode 1)))


;;dump-jump
;;flyspell
;;rinari
;tag?ctag?gtac?etag?
;;migemo
;;magit
;;git-gutter





(straight-use-package 'magit)
(straight-use-package 'forge)
(global-set-key (kbd "C-x g") 'magit-status)





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

;;hydra https://github.com/abo-abo/hydra

(straight-use-package 'yaml-mode)


;show-paren-mode
(setq show-paren-delay 0) ; default 0.125
(setq show-paren-style 'mixed) ; default 'parenthesis others 'expression 'mixed
(setq show-paren-when-point-inside-paren t) ; default nil
(setq show-paren-when-point-in-periphery t) ; default nil
(set-face-background 'show-paren-match "#def")
(set-face-foreground 'show-paren-match "#210")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)
;; alt smartparens.el
;; https://github.com/Fuco1/smartparens

;;https://github.com/Fuco1/smartparens

;括弧の自動挿入
;(electric-pair-mode 1)


;recentf

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




;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
;; https://qiita.com/marcy_o/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))


;;http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(setq make-backup-files nil)

;;http://emacs.rubikitch.com/rebecca-theme/
(straight-use-package 'rebecca-theme)
(load-theme 'rebecca t)



;;https://www.emacswiki.org/emacs/emacs-w3m
(straight-use-package 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

(setq
 company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend))


(provide 'init)
;;; init.el ends here
