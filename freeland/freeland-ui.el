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

(use-package emacs
  :init
  ;; set utf-8 environment
  (setq locale-coding-system 'utf-8)
  ;; set the startup frame size
  (setq initial-frame-alist
	'((height . 400) (width . 120)))
  ;; set the ruler if the input characters over 80
  (setq-default fill-column 80)
  ;; (setq frame-resize-pixelwise t)
  ;; (toggle-frame-maximized)
  ;; set the frame transparency of 90%, that better visual effects in dark theme
  ;; (set-frame-parameter (selected-frame) 'alpha '(95 95))
  ;; clean startup
  (setq inhibit-startup-message t
	inhibit-startup-echo-area-message "welcome to the Emacs world!"
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
  (load-theme 'modus-operandi-tritanopia t)
  (blink-cursor-mode -1)
  (global-hl-line-mode)
  (recentf-mode)
  ;; add directory includes other .info documents into the default Info directory
  (add-to-list 'Info-directory-list "~/Developer/Emacs-info/")
  ;; customize the display of modeline for major and minor modes
  :delight
  ;; change "Elisp/d" to "EL"
  (emacs-lisp-mode "EL")
  ;; completely hide the following modes
  (eldoc-mode)
  :bind
  ("C-x f" . #'recentf-open)
  ("C-x c" . #'display-fill-column-indicator-mode)
  ("C-c r" . #'restart-emacs)
  ("C-c b" . #'swap-browser)
  ("C-c p" . #'list-packages)
  ("C-c i" . #'ibuffer))

(provide 'freeland-ui)
;;; freeland-ui.el ends here
