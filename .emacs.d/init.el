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

(straight-use-package 'flymake-ruby)
;flymake
(straight-use-package 'robe)
;; M-x inf-ruby
;; M-x robe-start


(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r r") 'inf-ruby)

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(straight-use-package 'helm)
;(straight-use-package 'consel)
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(global-set-key (kbd "C-s") 'swiper)

(straight-use-package 'ruby-block)
(straight-use-package 'ruby-electric)
(straight-use-package 'pry)
(straight-use-package 'inf-ruby)
(straight-use-package 'flycheck)
(straight-use-package 'robe)

;;rbenv
;;ruby end
;;rubocop flycheck lint
;;projectile?
;;smart-newline.el
;;dump-jump
;;flyspell
;;rinari
;tag?
;;migemo
;;magit
(straight-use-package 'forge)
(straight-use-package 'magit)
;;yasnippet
;;git-company
;;neotree


(straight-use-package 'yaml-mode)


;show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

      
(tool-bar-mode -1) 

;https://emacs-jp.github.io/packages/anzu
(straight-use-package 'anzu) 
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
