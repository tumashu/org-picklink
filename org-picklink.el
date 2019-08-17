;;; org-picklink.el --- Pick a headline link from org-agenda      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Homepage: https://github.com/tumashu/org-picklink
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * org-picklink's README                        :README:

;; This package contains the command `org-picklink' which pops
;; up a org-agenda window as link chooser, user can
;; pick a headline in this org-agenda window, then insert
;; its link to origin org-mode buffer.

;; [[./snapshots/org-picklink.gif]]

;; The simplest installation method is to call:

;; #+begin_example
;; (define-key org-mode-map "\C-cj" 'org-picklink)
;; #+end_example

;;; Code:
;; * org-picklink's code                         :CODE:
(require 'subr-x)
(require 'org-agenda)

;;;###autoload
(defvar org-picklink-links nil
  "Record all links info.")

(defvar org-picklink-breadcrumbs-separator "/"
  "The separator used by org-picklink's breadcrumbs.")

(defvar org-picklink-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'org-picklink-quit-window)
    (define-key keymap (kbd "C-<return>") 'org-picklink-store-link)
    (define-key keymap (kbd "<return>") 'org-picklink-store-link-and-quit-window)
    keymap)
  "Keymap for org-picklink-mode.")

;;;###autoload
(defun org-picklink-store-link (&optional ignore-breadcrumbs)
  "Store id link of current headline.

If IGNORE-BREADCRUMBS is t, ignore breadcurmbs."
  (interactive "P")
  (let ((selected-string
         (when mark-active
           (buffer-substring-no-properties
            (region-beginning) (region-end)))))
    (deactivate-mark)
    (org-with-point-at (or (org-get-at-bol 'org-hd-marker)
		           (org-agenda-error))
      (let* ((id (concat "id:" (org-id-get (point) t)))
             (breadcrumbs
              (when (and (not ignore-breadcrumbs)
                         org-prefix-has-breadcrumbs)
                (let ((s (org-format-outline-path
                          (org-get-outline-path)
	                  (1- (frame-width))
	                  nil org-picklink-breadcrumbs-separator)))
                  (if (eq "" s) "" (concat s org-picklink-breadcrumbs-separator)))))
             (desc (or selected-string
                       (concat breadcrumbs
                               (org-entry-get (point) "ITEM")))))
        (push (list :link id :description desc :type "id") org-picklink-links)
        (message "Store link: [[%s][%s]]" (concat (substring id 0 9) "...") desc)))))

;;;###autoload
(defun org-picklink-quit-window ()
  "Quit org agenda window and insert links to org mode buffer."
  (interactive)
  (setq header-line-format nil)
  (org-picklink-mode -1)
  (org-agenda-quit)
  (setq org-picklink-links
        (reverse org-picklink-links))
  ;; When a link is found at point, insert ", "
  (when (save-excursion
          (let* ((end (point))
                 (begin (line-beginning-position))
                 (string (buffer-substring-no-properties
                          begin end)))
            (and (string-match-p "]]$" string)
                 (not (string-match-p ", *$" string)))))
    (insert ", "))
  (dolist (link org-picklink-links)
    (org-insert-link nil (plist-get link :link) (plist-get link :description))
    (pop org-picklink-links)
    (when org-picklink-links
      (cond ((org-in-item-p)
             (call-interactively #'org-insert-item))
            (t (insert " "))))))

;;;###autoload
(defun org-picklink-store-link-and-quit-window ()
  "Store link then quit org agenda window."
  (interactive)
  (call-interactively 'org-picklink-store-link)
  (org-picklink-quit-window))

;;;###autoload
(defun org-picklink (&optional search-tag)
  "Open org agenda window as a link selector.

if region is actived, ‘org-agenda’ will search string
in region and replace it with selected link.

When SEARCH-TAG is t, use `org-tags-view' instead
of `org-search-view'.

This command only useful in org mode buffer."
  (interactive "P")
  (if (not (derived-mode-p 'org-mode))
      (message "org-picklink works only in org-mode!")
    (let ((org-agenda-window-setup 'only-window)
          (buffer (current-buffer))
          (search-string
           (if mark-active
               (buffer-substring-no-properties
                (region-beginning) (region-end))
             "")))
      ;; Call org-agenda
      (when (and search-string (> (length search-string) 0))
        (delete-region (region-beginning) (region-end)))
      (if search-tag
          (org-tags-view nil search-string)
        (org-search-view
         nil
         (if (= (length search-string) 0)
             "*"
           search-string)))
      ;; Update `header-line-format'
      (with-current-buffer (get-buffer org-agenda-buffer)
        (org-picklink-mode 1)
        (setq header-line-format
              (format
               (substitute-command-keys
                (concat
                 "## Type `\\[org-picklink-store-link]' or `\\[org-picklink-store-link-and-quit-window]' "
                 "to push links to buffer \"%s\". ##"))
               (buffer-name buffer)))))))

(define-minor-mode org-picklink-mode
  "org picklink mode"
  nil " org-picklink")

(provide 'org-picklink)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-picklink.el ends here
