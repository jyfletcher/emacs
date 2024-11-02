
;;;;; Handy things ;;;;;
;; Fix the tilde, etc keymap issue - https://www.emacswiki.org/emacs/DeadKeys
(require 'iso-transl)
;; Present a list of recently opened files
(recentf-mode 1)
;; Save minibuffer input history
(setq history-length 25)
(savehist-mode 1)
;; Restore cursor position when opening a file
(save-place-mode 1)
;; Update buffer if underlying file changes
(global-auto-revert-mode 1)
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
;; Disable the toolbar / menu-bar
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Disable the scrollbar
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
;; Keep gconf from overriding config
(define-key special-event-map [config-changed-event] 'ignore)
;; Set up a personal load path
;; You can place a default.el file here
;; that will get parsed after this file
;; Not used this in a while
;(push "~/.emacs.d/lisp" load-path)
;; Enable global line wrapping
(global-visual-line-mode)
;; F12 to indent the whole buffer
(defun indent-buffer ()
  "Auto-indent buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

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

;; base64url functions
(defun base64-to-base64url (str)
  "Convert STR from base64 to base64url encoded."
  (setq str (replace-regexp-in-string "=+$" "" str))
  (setq str (replace-regexp-in-string "+" "-" str))
  (setq str (replace-regexp-in-string "/" "_" str)))

(defun base64url-to-base64 (str)
  "Convert STR from base64url encoded to base64."
  (setq str (replace-regexp-in-string "-" "+" str))
  (setq str (replace-regexp-in-string "_" "/" str))
  (let ((mod (% (length str) 4)))
    (cond
     ((= mod 1) (concat str "==="))
     ((= mod 2) (concat str "=="))
     ((= mod 3) (concat str "="))
     (t str))))

(defun base64url-encode-string (str)
  "Convert STR into base64url encoding."
  (base64-to-base64url (base64-encode-string str t)))

(defun base64url-decode-string (str)
  "Convert STR from base64url encoded."
  (base64-decode-string (base64url-to-base64 str)))


(defun base64url-decode-region (beg end)
  "Base64 decode marked region from BEG to END"
  (interactive "r")
  (save-excursion
    (let ((new-text (base64url-decode-string (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert new-text))))

(defun base64url-encode-region (beg end)
  "Base64 encode marked region from BEG to END"
  (interactive "r")
  (save-excursion
    (let ((new-text (base64url-encode-string (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert new-text))))

(defun jwt-decode-region (beg end)
  "Decode JWT in the marked region, from BEG to END."
  (interactive "r")
  (save-excursion
	(setq data (split-string (buffer-substring-no-properties beg end) "\\."))
	(let ((new-text (string-join (cons (base64url-decode-string (car data))
						  (list (base64url-decode-string (car (cdr data))))) ",")))
	  (kill-region beg end)
	  (insert new-text))))


;;; Load up packages
;; Specify all packages
(setq my-packages '(
					company
					company-quickhelp
					go-mode
					helm
					helm-swoop
					helm-xref
					inf-elixir
					magit
					markdown-mode
					mix
					monky
					nasm-mode
					org
					org-bullets
					plantuml-mode
					projectile
					restclient
					treesit-auto
					yaml-mode
					yasnippet
					zenburn-theme
					))
(setq package-archives '())
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;; Set exec-path and PATH
(setenv "PATH" (concat (getenv "PATH") ":/home/justin/.local/bin"))
(setq exec-path (append exec-path '("/home/justin/.local/bin")))

;;; Show lines and columns
(setq column-number-mode t)

;;;;; END handy things ;;;;;

;;;;; tree-sitter things ;;;;;

;;; Set up tree-sitter

 ;; Should use:
 ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; at least once per installation or while changing this list
(setq treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(bash "https://github.com/tree-sitter/tree-sitter-bash")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	))



;;;;; END tree-sitter things ;;;;;

;;;;; Restclient init ;;;;;

(require 'restclient)

;;;;; END Restclient init ;;;;;

;;;;; elixir config ;;;;;


(add-to-list 'major-mode-remap-alist
 '(elixir-mode . elixir-ts-mode))

;; Set up the elixir language server
; git clone https://github.com/elixir-lsp/elixir-ls.git
; cd elixir-ls
; mix deps.get
; MIX_ENV=prod mix compile
; MIX_ENV=prod mix elixir_ls.release -o ~/.local/bin

;;; Set up eglot
(use-package
 eglot
 :ensure t
 :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh")))


;;; Set up elixir-ts-mode
;; clone these repos
;; https://github.com/wkirschbaum/elixir-ts-mode
;; https://github.com/wkirschbaum/heex-ts-mode
(load "~/.emacs.d/heex-ts-mode/heex-ts-mode.el")
(load "~/.emacs.d/elixir-ts-mode/elixir-ts-mode.el")

(use-package
  elixir-ts-mode
  :ensure t
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))

;; Set up mix minor mode
(use-package mix
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode))

;; Set up inf-elixir
;; https://github.com/J3RN/inf-elixir
(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module)))



;;;;; END elxir things ;;;;;

;;;;; Erlang things ;;;;;

(setq load-path (cons  "/usr/local/lib/erlang26/lib/tools-3.6/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang26/lib/")
;(setq exec-path (cons "/usr/local/otp/bin" exec-path))
(require 'erlang-start)

;;;;; END Erlang things ;;;;;

;;;; Comany mode things ;;;;;
;;(use-package company
;;  :hook (elixir-ts-mode . company-mode))
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode)
;;;; End Company things

;;;; Go things ;;;;
;; Optional: load other packages before eglot to enable eglot integrations.
(require 'company)
(require 'yasnippet)
(require 'project)
(require 'go-mode)
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)

(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

(add-hook 'before-save-hook
    (lambda ()
        (call-interactively 'eglot-code-action-organize-imports))
    nil t)

;;;; END Go things ;;;;


;;;;; Package config ;;;;;
(require 'magit)

;; OrgMode
;; https://orgmode.org
;; M-x package-install org
;; M-x package-install org-bullets
(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-log-done 'time)
(setq org-ellipsis " ▼")
(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;;; Helm and Swoop
;; Helm - https://github.com/emacs-helm/helm
;; Swoop - https://github.com/ShingoFukuyama/helm-swoop
;; M-x package-install helm
;; M-x package-install helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
				
(helm-mode)
(require 'helm-xref)
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

;; eshell-helm
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell-cmpl-initialize)
	    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
	    (define-key eshell-mode-map (kbd "C-c f") 'helm-eshell-prompts-all)
	    (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
;; Load theme
(load-theme 'zenburn t)

;; Set font
(set-frame-font "Hack 8" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(inf-elixir mix monky cmake-mode zenburn-theme yaml-mode restclient projectile plantuml-mode org-bullets markdown-mode magit helm-xref helm-swoop company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
