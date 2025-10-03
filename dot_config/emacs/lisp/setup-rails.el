;;; setup-rails.el --- Rails固有の設定とキーバインド -*- lexical-binding: t -*-

;;; Commentary:
;; Rails開発に特化した設定、キーバインド、ユーティリティ関数

;;; Code:

;; Rails固有のキーバインド設定
(defun setup-rails-keybindings ()
  "Rails開発用のキーバインドを設定"
  (local-set-key (kbd "C-c r c") 'rails-console)
  (local-set-key (kbd "C-c r s") 'rails-server)
  (local-set-key (kbd "C-c r t") 'rspec-verify-single)
  (local-set-key (kbd "C-c r T") 'rspec-verify-all)
  (local-set-key (kbd "C-c r r") 'rspec-rerun)
  (local-set-key (kbd "C-c r g") 'rails-generate)
  (local-set-key (kbd "C-c r m") 'rails-migrate)
  (local-set-key (kbd "C-c r d") 'rails-dbconsole)
  (local-set-key (kbd "C-c r f") 'rails-find-file-at-point))

;; Rails開発用の便利関数
(defun rails-find-file-at-point ()
  "カーソル位置のファイル名に基づいてRailsファイルを開く"
  (interactive)
  (let ((filename (thing-at-point 'filename)))
    (when filename
      (cond
       ;; Model -> View
       ((string-match "app/models/\\(.+\\)\\.rb$" filename)
        (find-file (concat "app/views/" (match-string 1 filename))))
       ;; Controller -> View
       ((string-match "app/controllers/\\(.+\\)_controller\\.rb$" filename)
        (find-file (concat "app/views/" (match-string 1 filename))))
       ;; View -> Controller
       ((string-match "app/views/\\(.+\\)/[^/]+$" filename)
        (find-file (concat "app/controllers/" (match-string 1 filename) "_controller.rb")))
       (t (projectile-find-file))))))

(defun rails-insert-binding-pry ()
  "現在行にbinding.pryを挿入"
  (interactive)
  (end-of-line)
  (newline)
  (insert "binding.pry")
  (ruby-indent-line))

(defun rails-remove-binding-pry ()
  "ファイル内のbinding.pryを削除"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*binding\\.pry[[:space:]]*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun rails-toggle-spec-and-target ()
  "specファイルと実装ファイルを切り替え"
  (interactive)
  (let ((current-file (buffer-file-name)))
    (cond
     ;; spec -> implementation
     ((string-match "spec/\\(.+\\)_spec\\.rb$" current-file)
      (let ((impl-file (concat "app/" (match-string 1 current-file) ".rb")))
        (if (file-exists-p impl-file)
            (find-file impl-file)
          (find-file (concat "lib/" (match-string 1 current-file) ".rb")))))
     ;; implementation -> spec
     ((string-match "app/\\(.+\\)\\.rb$" current-file)
      (find-file (concat "spec/" (match-string 1 current-file) "_spec.rb")))
     ((string-match "lib/\\(.+\\)\\.rb$" current-file)
      (find-file (concat "spec/" (match-string 1 current-file) "_spec.rb")))
     (t (message "Not a Ruby file or spec file")))))

(defun rails-open-gemfile ()
  "Gemfileを開く"
  (interactive)
  (let ((gemfile (locate-dominating-file default-directory "Gemfile")))
    (if gemfile
        (find-file (concat gemfile "Gemfile"))
      (message "Gemfile not found"))))

(defun rails-bundle-install ()
  "bundle installを実行"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (async-shell-command "bundle install" "*bundle install*")))

(defun rails-routes ()
  "rails routesを実行して結果を表示"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (async-shell-command "rails routes" "*Rails Routes*")))

;; Rails固有のスニペット定義
(defun rails-insert-model-template ()
  "Railsモデルのテンプレートを挿入"
  (interactive)
  (let ((class-name (capitalize (file-name-base (buffer-file-name)))))
    (insert (format "class %s < ApplicationRecord\nend" class-name))))

(defun rails-insert-controller-template ()
  "Railsコントローラーのテンプレートを挿入"
  (interactive)
  (let ((class-name (replace-regexp-in-string "_controller" "Controller" 
                      (capitalize (file-name-base (buffer-file-name))))))
    (insert (format "class %s < ApplicationController\nend" class-name))))

;; Projectile Rails最適化
(defun setup-rails-projectile ()
  "ProjectileをRails用に最適化"
  (setq projectile-rails-custom-server-command "rails server")
  (setq projectile-rails-vanilla-command "rails")
  ;; Rails固有のファイルタイプ認識
  (add-to-list 'projectile-other-file-alist '("rb" "erb"))
  (add-to-list 'projectile-other-file-alist '("erb" "rb")))

;; Rails用の追加キーバインド
(global-set-key (kbd "C-c R p") 'rails-insert-binding-pry)
(global-set-key (kbd "C-c R P") 'rails-remove-binding-pry)
(global-set-key (kbd "C-c R t") 'rails-toggle-spec-and-target)
(global-set-key (kbd "C-c R g") 'rails-open-gemfile)
(global-set-key (kbd "C-c R b") 'rails-bundle-install)
(global-set-key (kbd "C-c R r") 'rails-routes)

;; Projectile Rails mode hooks  
(add-hook 'projectile-rails-mode-hook 'setup-rails-keybindings)
(add-hook 'projectile-rails-mode-hook 'setup-rails-projectile)

(provide 'setup-rails)
;;; setup-rails.el ends here