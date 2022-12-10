;;; init-parens.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-parens.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d parenthesis smartparens delete-block
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes parenthesis smartparens delete-block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; SmartParensPac
(use-package smartparens
  :hook ((prog-mode . smartparens-mode))
  :init (smartparens-global-mode t)
  ;;:diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-S-w" . sp-copy-sexp)
   ("C-M-k" . sp-change-enclosing)
   ;; ("M-k" . sp-kill-sexp)
   ;; ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ;; ("C-S-<backspace>" . sp-splice-sexp-killing-around)
   ("C-]" . sp-select-next-thing-exchange))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (progn (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
         (sp-local-pair 'org-mode "[" nil :actions nil)
         ;; https://github.com/Fuco1/smartparens/wiki/Permissions#insertion-specification
         ;; (sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

         (defun my-create-newline-and-enter-sexp (&rest _ignored)
           "Open a new brace or bracket expression, with relevant newlines and indent. "
           (newline)
           (indent-according-to-mode)
           (forward-line -1)
           (indent-according-to-mode)))
  )
;; -SmartParensPac

(provide 'init-smartparens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-smartparens.el ends here
