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
(straight-use-package 'robe)
;; M-x inf-ruby
;; M-x robe-start


(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r r") 'inf-ruby)

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(straight-use-package 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

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
;;migemo

(straight-use-package 'yaml-mode)
