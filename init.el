;;; init.el --- Summary
;; License: Public Domain - https://wiki.creativecommons.org/wiki/Public_domain
;;; Commentary:
;; My Emacs init.el file
;;; Code:

;; Some initial nice things
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
;; Turn beep off
(setq visible-bell nil)
;; Context when opening files, etc.
(ido-mode t)
;; 8 char tabs are annoying
(setq-default tab-width 4)
;; No splash screen
(setq inhibit-splash-screen t)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable the scrollbar
(toggle-scroll-bar -1)
;; Keep gconf from overriding config
(define-key special-event-map [config-changed-event] 'ignore)

;; Move line
(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Window resize
;; https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-f") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-b") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-p") 'shrink-window)
(global-set-key (kbd "S-C-n") 'enlarge-window)

;; Package repo configuration
;; Standard package.el + MELPA setup
;; (See also: https://github.com/milkypostman/melpa#usage)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)
;; Auto Package Update
(require 'auto-package-update)

;; Some other stuff
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)

; auto-complete
(require 'auto-complete)
(setq
  ac-auto-start 2
  ac-override-local-map nil
  ac-use-menu-map t
  ac-candidate-limit 40)
(ac-config-default)
(ac-flyspell-workaround)

;; Copy as format
;; M-x package-install copy-as-format
;; https://github.com/sshaw/copy-as-format/
(require 'copy-as-format)

;; Truncate the eshell buffer every 5 seconds
(setq eshell-buffer-maximum-lines 12000)
(defun eos/truncate-eshell-buffers ()
  "Truncates all eshell buffers."
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))
;; After being idle for 5 seconds, truncate all the eshell-buffers if
;; needed. If this needs to be canceled, you can run `(cancel-timer
;; eos/eshell-truncate-timer)'
(setq eos/eshell-truncate-timer
      (run-with-idle-timer 5 t #'eos/truncate-eshell-buffers))

;; highlight words
;;(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; Magit - git support
;; M-x install-package magit
;; Note that 'git' must be installed
(require 'magit)


;; Python mode settings
(setq py-python-command "/usr/bin/python3")
(defcustom python-shell-interpreter "python3"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
;(setq py-python-command "python3")

; jedi settings
(require 'jedi)

;; Standard Jedi.el setting
; This was part of the jedi docs so disabling it until needed
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; if you need to change your python intepreter
;(setq jedi:server-command
;  '("python3" "/home/dkjuyafl/.emacs.d/elpa/jedi-core-20170121.1410/jediepcserver.py"))
;(setq jedi:server-args
;    (list "--sys-path" "/home/justin/Projects/iniscripts/src"))
;(setq jedi:environment-root "jedi")  ; or any other name you like
;(setq jedi:environment-virtualenv
;      (append python-environment-virtualenv
;              '("--python" "/usr/bin/python3")))



(add-hook 'python-mode-hook
  (lambda ()
    (jedi:setup)
    (jedi:ac-setup)
    (local-set-key "\C-cd" 'jedi:show-doc)
    (local-set-key (kbd "M-SPC") 'jedi:complete)
    (local-set-key (kbd "M-.") 'jedi:goto-definition)))
    (add-hook 'python-mode-hook 'auto-complete-mode)


;; Monky - like magit but for hg (aka mercurial)
;; M-x package-install monky
(require 'monky)
(setq monky-process-type 'cmdserver)

;; This is for when you have issues with exec-path not working right
;; when starting from the GUI (not from a terminal)
;;
;; Allow emacs to access and exec from the PATH
;(defun set-exec-path-from-shell-PATH ()
;  (let ((path-from-shell (replace-regexp-in-string
;                          "[ \t\n]*$"
;                          ""
;                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;    (setenv "PATH" path-from-shell)
;    (setq eshell-path-env path-from-shell) ; for eshell users
;    (setq exec-path (split-string path-from-shell path-separator))))
;(when window-system (set-exec-path-from-shell-PATH))


;; Go Language
;; Download these
;; go get -v -u github.com/rogpeppe/godef
;; go get -v -u github.com/nsf/gocode
;; go get -v -u github.com/jstemmer/gotags
;; and M-x package-install these
;; go-mode go-eldoc auto-complete go-autocomplete go-direx popwin
;; and 'eval' these commands (with GOPATH set properly)
;; or else the bins downloaded above will not be useful
;; (setenv "GOPATH" "/home/user/Projects/mygoproject")
;; (setenv "PATH" (concat (getenv "GOPATH") "/bin" path-separator (getenv "PATH")))
;; (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(require 'go-mode)
;(add-to-list 'exec-path "/home/dkjuyafl/opt/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'yas-minor-mode)
(defun go-mode-setup ()
  "Set up go-eldoc."
  (go-eldoc-setup))
(add-hook 'go-mode-hook 'go-mode-setup)
(require 'go-autocomplete)
(require 'go-direx)
(define-key go-mode-map (kbd "C-c C-k") 'go-direx-pop-to-buffer)
(define-key go-mode-map (kbd "<M-up>") 'move-line-up)
(define-key go-mode-map (kbd "<M-down>") 'move-line-down)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*go-direx:" :regexp t :position right :width 0.4 :dedicated
		t :stick t) popwin:special-display-config)


;; Buffer Move
;; M-x package-install buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; Erlang mode
;; M-x package-install erlang
(require 'erlang-start)

;; Restclient
;; M-x package-install restclient
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))


;; YaML
;; M-x package-install yaml-mode
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
    (lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; Markdown
;; M-x package-install markdown-mode
(require 'markdown-mode)

;; OrgMode
;; https://orgmode.org
;; M-x package-install org
;; M-x package-install org-bullets
(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Helm and Swoop
;; Helm - https://github.com/emacs-helm/helm
;; Swoop - https://github.com/ShingoFukuyama/helm-swoop
;; M-x package-install helm
;; M-x package-install helm-swoop
(require 'helm-config)
(require 'helm-swoop)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
				
(helm-mode 1)
;; Swoop keybinds
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)


;; Web Mode
;; M-x package-install web-mode
;; https://github.com/sabof/org-bullets
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; Load theme
(load-theme 'zenburn t)

;; Custom Set Variables -- emacs manages this section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
	("d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" default)))
 '(package-selected-packages
   (quote
	(copy-as-format web-mode org-bullets helm-swoop helm org markdown-mode restclient popwin go-autocomplete go-eldoc zenburn-theme yasnippet yaml-mode python-mode monky magit jedi go-mode flycheck erlang autopair)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :height 112 :width normal :foundry "DAMA" :family "Ubuntu Mono")))))

(provide '.emacs)
;;; init.el ends here


