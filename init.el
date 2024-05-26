;;; init.el --- starting point
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

;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-threshold (* 50 1000 1000))

;; the freeland directory location
;; use 'defvar' here because 'freeland-dir' is dynamic variable determined at
;; runtime
(defvar freeland-dir "~/.emacs.d/freeland/")

;; put the freeland directory into 'load-path'
(add-to-list 'load-path freeland-dir)

;; OS verification
(defconst IS-MAC (eq system-type 'darwin))
;; (defconst IS-LINUX (eq system-type 'gnu/linux))
;; (defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; main keybindings for MacOS
(when IS-MAC
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-control-modifier 'control))

;; Emacs GUI version
(if (display-graphic-p)
    (progn
      (setq custom-file "~/.emacs.d/custom.el")
      (load custom-file)
      (require 'freeland-ui)
      (require 'freeland-packages)
      ;; (load-theme 'leuven-dark t)
      (load-theme 'spacemacs-light t)
      ;; put customization (custom-set-variables) into custom.el
      ))

;; use tree-sitter library to parse C or C++ source code
;; (setq major-mode-remap-alist
;;       '((c-mode . c-ts-mode)
;; 	))

;; (setq c-default-style "stroustrup")

(message "Emacs startup time is %s" (emacs-init-time))

(setq gc-cons-threshold (* 2 1000 1000))
;; (garbage-collect)

(provide 'init)
;;; init.el ends here
