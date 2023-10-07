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

(setq gc-cons-threshold most-positive-fixnum)

;; the freeland directory location
;; use 'defvar' here because 'freeland-dir' is dynamic variable determined at
;; runtime
(defvar freeland-dir "~/.emacs.d/freeland/")

;; load all sub-dirs under parent-dir recursively
(defun freeland-add-subfolders-to-loadpath (parent-dir)
  "Add all level PARENT-DIR subdirs to the \\'load-path\\'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (freeland-add-subfolders-to-loadpath name)))))

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

(require 'freeland-packages)
(require 'freeland-ui)

;; put customization (custom-set-variables) into custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make GC pauses faster by decreasing the threshold
(setq gc-cons-threshold (* 1024 1024 64))
(garbage-collect)

(message "Emacs startup time is %s" (emacs-init-time))

(provide 'init)
;;; init.el ends here
