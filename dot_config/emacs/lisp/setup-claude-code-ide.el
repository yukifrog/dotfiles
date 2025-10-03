;;; setup-claude-code-ide.el --- Claude Code IDE integration with MCP support -*- lexical-binding: t -*-

;;; Commentary:
;; claude-code-ide.el integration for native MCP-based Claude Code integration
;; Provides LSP integration, Tree-sitter support, and bidirectional communication

;;; Code:

;; vterm for terminal support (recommended for claude-code-ide)
(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  ;; Setup Emacs tools for MCP integration
  (claude-code-ide-emacs-tools-setup)

  ;; Additional key bindings
  (define-key global-map (kbd "C-c i c") #'claude-code-ide-start)
  (define-key global-map (kbd "C-c i k") #'claude-code-ide-kill)
  (define-key global-map (kbd "C-c i r") #'claude-code-ide-restart)
  (define-key global-map (kbd "C-c i s") #'claude-code-ide-send-buffer)
  (define-key global-map (kbd "C-c i p") #'claude-code-ide-send-region))

;; Direct Claude Code IDE message sending
(defun claude-code-ide-send ()
  "Send message using Claude Code IDE"
  (interactive)
  (cond
   ;; If claude-code-ide is loaded and has an active session with send message function
   ((and (featurep 'claude-code-ide)
         (fboundp 'claude-code-ide-send-message)
         (claude-code-ide-session-active-p))
    (call-interactively 'claude-code-ide-send-message))
   ;; If claude-code-ide menu is available, use that
   ((and (featurep 'claude-code-ide)
         (fboundp 'claude-code-ide-menu))
    (call-interactively 'claude-code-ide-menu))
   ;; If no Claude Code IDE, start it first
   ((and (featurep 'claude-code-ide)
         (fboundp 'claude-code-ide-start))
    (claude-code-ide-start)
    (message "Claude Code IDE started. Try again in a moment."))
   (t
    (message "Claude Code IDE not available. Use C-c C-' to open menu."))))

;; Update keybinding to use Claude Code IDE only
(define-key global-map (kbd "C-c c m") #'claude-code-ide-send)

;; Helper function to check if we're in a Claude Code IDE session
(defun claude-code-ide-session-active-p ()
  "Check if Claude Code IDE session is active in current project"
  (and (featurep 'claude-code-ide)
       (fboundp 'claude-code-ide-process-running-p)
       (claude-code-ide-process-running-p)))

;; Status line integration
(defun claude-code-ide-status ()
  "Return status string for modeline"
  (if (claude-code-ide-session-active-p)
      " [Claude-IDE]"
    ""))

;; Enhanced project integration
(defun claude-code-ide-auto-start-if-project ()
  "Auto-start Claude Code IDE if in a project and not already running"
  (when (and (project-current)
             (featurep 'claude-code-ide)
             (fboundp 'claude-code-ide-start)
             (not (claude-code-ide-session-active-p)))
    (claude-code-ide-start)))

;; Add hook for auto-starting in projects (optional)
;; (add-hook 'find-file-hook 'claude-code-ide-auto-start-if-project)

(provide 'setup-claude-code-ide)
;;; setup-claude-code-ide.el ends here