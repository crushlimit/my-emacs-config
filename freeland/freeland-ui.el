;;; freeland-ui.el --- UI tweaks and package repos relocate.
;;
;; Copyright © 2023
;;
;; Author: Jacky Young

;; This file is not part of GNU Emacs.

;;; Commentary:

;; the starting point

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

;; Swap different browsers between Safari and Eww for viewing the web pages
(defun swap-browser ()
  "Swap browser between Safari and Eww."
  (interactive)
  (if (eq browse-url-browser-function 'browse-url-default-macosx-browser)
      (progn
        (setq browse-url-browser-function 'eww)
        (message "Browser is now Emacs EWW."))
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
    (message "Browser is now MacOS SAFARI.")))

;; Set default fonts, CJK fonts and emoji fonts
(defun my/set-fonts (default-font-name
                     default-font-height
                     cjk-font-name
                     cjk-font-scale
                     emoji-font-name
                     emoji-font-scale)
  "Helper function to set default, cjk and emoji fonts."
  ;; Set the default font
  (when (member default-font-name (font-family-list))
    (set-face-attribute 'default nil
                        :family default-font-name
                        :height default-font-height)
    (set-frame-font default-font-name nil t))

  ;; Set the CJK font in the default fontset.
  (when (member cjk-font-name (font-family-list))
    (dolist (script (list 'han 'kana 'cjk-misc))
      (set-fontset-font t script cjk-font-name)))

  ;; Set the Emoji font in the default fontset.
  (when (member emoji-font-name (font-family-list))
    (set-fontset-font t 'emoji emoji-font-name))

  ;; Rescale the CJK and emoji fonts.
  (setq face-font-rescale-alist
        `((,(format ".*%s.*" cjk-font-name) . ,cjk-font-scale)
          (,(format ".*%s.*" emoji-font-name) . ,emoji-font-scale))))

;; emacs configuration tweaks
(use-package emacs
  :init
  ;; set utf-8 environment
  (setq locale-coding-system 'utf-8)
  ;; set the startup frame size
  (setq initial-frame-alist
	'((height . 350) (width . 120)))
  ;; set the ruler if the input characters over 80
  (setq-default fill-column 80)
  ;; (setq frame-resize-pixelwise t)
  ;; (toggle-frame-maximized)
  ;; set the frame transparency of 90%, that better visual effects in dark theme
  ;; (set-frame-parameter (selected-frame) 'alpha '(95 95))
  ;; clean startup
  (setq inhibit-startup-message t
	;; scratch buffer defaults to be org-mode
	initial-major-mode 'org-mode
	;; initial-major-mode 'emacs-lisp-mode
	;; initial-major-mode 'common-lisp-mode
	;; initial-scratch-message ";; Happy Hacking in Emacs...")
	initial-scratch-message nil)
  (setq isearch-lazy-count t)
  (column-number-mode)
  (tool-bar-mode -1)
  (global-display-line-numbers-mode 1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  ;; (desktop-save-mode 1) -- this will cause Emacs to crash (I don't know why)
  ;; (load-theme 'leuven t)
  ;; Different computers might need different scaling factors with same fonts.
  (cond
   ;; If MacOS
   ((eq system-type 'darwin)
    (my/set-fonts
     "JetBrains Mono" 140
     "Source Han Serif SC" 1.15
     "Apple Color Emoji" 0.9)))
  ;; turn off the annoying bell and make the world quiet.
  (setq ring-bell-function 'ignore)
  ;; use extended and enhanced perl mode
  (fset 'perl-mode 'cperl-mode)
  ;; add directory includes other .info documents into the default Info directory
  (add-to-list 'Info-directory-list "~/Developer/Emacs-info/")
  (add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
  (add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
  ;; customize the display of modeline for major and minor modes
  :delight
  ;; change "Elisp/d" to "EL"
  (emacs-lisp-mode "EL")
  ;; completely hide the following modes
  (eldoc-mode)
  ;; set C language default indentation style
  ;; (defvar c-indentation-style "k&r")
  :bind
  ("C-x f" . #'recentf-open)
  ;; ("C-x c" . #'display-fill-column-indicator-mode)
  ("C-x C-p" . #'cperl-perldoc)
  ("C-x e" . #'eww)
  ("C-c c" . #'compile)
  ("C-c r" . #'restart-emacs)
  ("C-c b" . #'swap-browser)
  ("C-c p" . #'list-packages)
  ("C-c i" . #'ibuffer)
  ("C-c e" . #'eshell)
  ("C-c v" . #'vterm)
  ("C-c f" . #'avy-goto-char)
  ("C-c l" . #'avy-goto-line))

(provide 'freeland-ui)
;;; freeland-ui.el ends here
