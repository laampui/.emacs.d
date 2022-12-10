;; init-project.el --- Initialize project configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Project configurations.
;;

;;; Code:

;; Git

;; (define-key project-prefix-map "m" #'magit-project-status)
;; (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)

(use-package find-file-in-project
  :if (executable-find "find")
  :init
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t)))

;; Manage and navigate projects
(use-package project
  :ensure nil
  :bind
  (
   :map project-prefix-map
   ;; ("f" . ffip)
   ("f" . project-find-file)
   ;; ("f" . projectile-find-file)
   ("m" . magit-project-status)
   ("v" . bs/project-vterm)
   ("r" . consult-ripgrep)
   )
  :config
  (defun bs/project-vterm ()
    "Opens a new vterm buffer at project root."
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current)))
           (project-root-name (s-replace-all '(("." . "")) (f-filename default-directory)))
           (vterm-buffer-name (format "*%s-vterm*" project-root-name)))
      (vterm)))
  :custom
  (project-switch-commands
   '(
     ;; (ffip "Find file")
     (project-find-file "Find file")
     ;; (projectile-find-file "Find file")
     (consult-ripgrep "Find regexp")
     (project-find-dir "Find directory")
     (magit-project-status "Magit")
     (bs/project-vterm "Vterm")
     )
   )
  )

(provide 'init-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-project.el ends here
