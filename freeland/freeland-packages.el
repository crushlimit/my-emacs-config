;;; freeland-packages.el --- install core packages and configuration for packages
;;
;; Copyright © 2023
;;
;; Author: Jacky Young

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'package)

;; emacs package repositories
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; configuration on packages ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;; --- ace-window --- ;;;;;;
(use-package ace-window
  :ensure t
  :bind
  (("s-o" . ace-window)
   ("s-m" . ace-swap-window)
   ("s-=" . balance-windows))
  :init
  ;; (define-prefix-command 'ace-user-keymap)
  ;; (global-set-key (kbd "s-w") 'ace-user-keymap)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;;;;; --- all-the-icons --- ;;;;;;
(use-package all-the-icons
  :ensure t
  :demand t)

 ;;;;;; --- all-the-icons-completion --- ;;;;;;
(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  ;; (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))

;;;;;; --- company --- ;;;;;;
(use-package company
  :ensure t
  :demand t
  :init
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode)
  :delight)

;;;;;; --- consult --- ;;;;;;
(use-package consult
  :ensure t
  :bind
  ("C-c M-x" . consult-mode-command)
  ("C-x b" . consult-buffer)
  ;; ("C-x i" . consult-imenu)
  ("C-x ." . isearch-forward-thing-at-point)
  ;; M-g bindings in 'goto-map'
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flycheck)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

;;;;;; --- easy-hugo --- ;;;;;;
(use-package easy-hugo
  :ensure t
  :bind
  ("C-c h" . easy-hugo)
  :init
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-url "https://crushlimit.github.io")
  (setq easy-hugo-root "~/Blogs/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-basedir "~/Blogs/Website")
  (setq easy-hugo-postdir "content/posts")
  (setq easy-hugo-org-header t)
  )

;; `ox-hugo' configuration
(use-package ox-hugo
  :ensure t
  :after ox)

;;;;;; --- elfeed --- ;;;;;;
(use-package elfeed
  :ensure t
  :bind
  ("C-c w" . elfeed)
  :init
  (setq elfeed-feeds
	'(("https://planet.emacslife.com/atom.xml" emacs english)
          ("https://chinadigitaltimes.net/chinese/feed/" politics chinese)
          ("https://yibaochina.com/?feed=rss2" politics chinese)
          ("https://cn.nytimes.com/rss/" news chinese)
          ("http://www.bbc.co.uk/zhongwen/simp/index.xml" news chinese)
          ("http://feeds.feedburner.com/reuters/CNTopNews" news chinese)
          ("https://news.ycombinator.com/rss" tech hacker english)
          ("https://nullprogram.com/feed/" programming english)
          ("https://stallman.org/rss/rss.xml" rms english)
	  ("https://www.solidot.org/index.rss" tech chinese)
          ))
  (setq-default elfeed-search-filter "@3-days-ago +unread")
  (setf url-queue-timeout 30))

;;;;;; --- elisp-demos --- ;;;;;;
(use-package elisp-demos
  :ensure t)

;;;;;; --- exec-path-from-shell --- ;;;;;;
;;; ensure environment variables inside Emacs the same as in the user's shell.
;;; if `(getenv "SHELL ")` in Emacs points at 'bash' or 'zsh', it works fine.
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;;;;; --- flycheck --- ;;;;;;
;; it's better set 'flycheck-emacs-lisp-load-path' to inherit from 'load-path'
;; to bypass flycheck error: "cannot open load file: no such file or directory"
;; this can be customized directly from 'customize-group'
(use-package flycheck
  :ensure t
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (lisp-mode . flycheck-mode)
  (cperl-mode . flycheck-mode)
  (flycheck-mode . flycheck-color-mode-line-mode)
  ;; inline-mode: the diagnostic info displays just under the highlighted line
  ;; (flycheck-mode . flycheck-inline-mode)
  :init
  (flycheck-mode))

;;;;;; --- helpful --- ;;;;;;
(use-package helpful
  :ensure t
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h ." . helpful-at-point)
   ("C-h f" . helpful-function)
   ("C-h c" . helpful-callable)
   ("C-h r" . helpful-macro)
   ("C-h <tab>" . info-display-manual))
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :requires
  (elisp-demos))

;;;;;; --- lsp --- ;;;;;;
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (cperl-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  (lsp lsp-deferred)
  :config
  ;; use 'PLS' as the default language server of Perl, which is the best one of
  ;; three ('PLS', 'PerlNavigator', 'Perl::LanguageServer'
  (setq lsp-pls-executable "~/perl5/bin/pls"))

;;;;;; --- magit --- ;;;;;;
(use-package magit
  :ensure t
  :defer t
  :init (setq magit-refresh-status-buffer nil)
  :bind (("C-x g" . magit-status)))

;;;;;; --- modus theme --- ;;;;;;
(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-org-blocks 'gray-background))

;;;;;; --- marginalia --- ;;;;;;
(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;;;;;; --- markdown --- ;;;;;;
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;;;;; --- markdown-preview-mode --- ;;;;;;
(use-package markdown-preview-mode
  :ensure t
  :requires (markdown-mode)
  :hook
  (gfm-mode . markdown-preview-mode))

;;;;;; --- orderless --- ;;;;;;
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;;;;;; --- org mode --- ;;;;;;
(use-package org
  :ensure t
  :init
  (setq org-startup-indented t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "VERIFY(v)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces
	'(("VERIFY" . "orange")
	  ("CANCELED" . (:foreground "gray" :weight bold))))
  (setq org-log-done 'time)
  (org-babel-do-load-languages 'org-babel-load-languages '((lisp . t))))

;;;;;; --- paredit --- ;;;;;;
(use-package paredit
  :ensure t
  :demand
  :hook
  (emacs-lisp-mode . paredit-mode)
  (ielm-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (slime-mode . paredit-mode)
  (slime-repl-mode . paredit-mode)
  ;; (cperl-mode . paredit-mode)
  :delight)

;;;;;; --- savehist --- ;;;;;;
;; persist history over Emacs restarts. Verico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;;;;;; --- slime --- ;;;;;;
(use-package slime
  :ensure t
  :bind
  ("C-c s" . slime)
  :init
  (setq slime-lisp-implementations
	'(
	  ;; specify SBCL implementation
	  (sbcl ("/opt/homebrew/Cellar/sbcl/2.3.4/bin/sbcl")
		:coding-system utf-8-unix)
	  ;; specify CLISP implementation
	  (clisp ("/opt/homebrew/Cellar/clisp/2.49.92_1/bin/clisp"))))
  (setq slime-auto-select-connection 'always)
  (setq common-lisp-hyperspec-root
	"file:///Users/jacky_goodluck/Developer/Programming/LispPrograms/HyperSpec-7-0/HyperSpec/")
  (slime-setup '(slime-fancy slime-company slime-repl))
  :preface
  (set-default 'auto-mode-alist
	       (append '(("\\.lisp\\'" . lisp-mode)
			 ("\\.lsp\\'" . lisp-mode)
			 ("\\.cl\\'" . lisp-mode))
		       auto-mode-alist)))

;;;;;; --- undo-tree --- ;;;;;;
(use-package undo-tree
  :ensure t
  :bind
  ("C-c u" . #'undo-tree-visualize)
  :init
  (global-undo-tree-mode)
  :delight)

;;;;;; --- vertico --- ;;;;;;
(use-package vertico
  :ensure t
  :custom
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (vertico-mode))

;;;;;; --- which-key --- ;;;;;;
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;;;;; --- white-mode --- ;;;;;;
;; highlight the parts of line exceeding the 'fill-column' 80 limit with magenta
(use-package whitespace
  :ensure t
  :init
  (setq whitespace-line-column 81)
  (setq whitespace-style '(face lines-tail))
  :hook
  ((prog-mode text-mode gfm-mode) . whitespace-mode))

;;;;;; --- with-editor --- ;;;;;;
(use-package with-editor
  :ensure t
  :init
  (keymap-global-set "<remap> <async-shell-command>"
                     #'with-editor-async-shell-command)
  (keymap-global-set "<remap> <shell-command>"
                     #'with-editor-shell-command))

;;;;;; --- yasnippet --- ;;;;;;
  (use-package yasnippet
    :ensure t
    :hook
    ((text-mode prog-mode conf-mode snippet-mode) . yas-minor-mode-on)
    :init
    (defvar yas-snippet-dir "~/.emacs.d/snippets"))

(provide 'freeland-packages)
;;; freeland-packages.el ends here
