;; init-tabs.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 ema2159

;; Author: ema2159 <ema2159@gmail.com>
;; URL: https://github.com/ema2159

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
;; tabs is smarter tab solution for Emacs, it sort tab with using frequency.
;;

;;; Code:
(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-enable-key-bindings t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons nil)
  (setq centaur-tabs-gray-out-icons 'buffer)
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-group-by-projectile-project)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("s-q" . centaur-tabs-backward)
  ("s-e" . centaur-tabs-forward)
  ("s-t" . centaur-tabs--create-new-tab)
  ("s-c" . centaur-tabs--kill-this-buffer-dont-ask))

(provide 'init-tabs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tabs.el ends here
