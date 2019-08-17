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
(require 'org)

;;;###autoload
(defvar org-picklink-info (make-hash-table)
  "A hashtable recording buffer, buffer-window and window point.")

(defvar org-picklink-breadcrumbs-separator "/"
  "The separator used by org-picklink's breadcrumbs.")

(defvar org-picklink-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'org-picklink-quit-window)
    (define-key keymap (kbd "C-<return>") 'org-picklink-push-link)
    (define-key keymap (kbd "<return>") 'org-picklink-push-link-and-quit-window)
    keymap)
  "Keymap for org-picklink-mode.")

;;;###autoload
(defun org-picklink-push-link ()
  "Push link of current headline to buffer in `org-picklink-info'."
  (interactive)
  (org-agenda-check-no-diary)
  (when-let* ((target-buffer (gethash :buffer org-picklink-info)))
    (let* ((inhibit-read-only t)
           (selected-string
            (when mark-active
              (buffer-substring-no-properties
               (region-beginning) (region-end))))
           (hdmarker (or (org-get-at-bol 'org-hd-marker)
		         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (breadcrumbs
            (when org-prefix-has-breadcrumbs
              (org-with-point-at (org-get-at-bol 'org-marker)
	        (let ((s (org-format-outline-path
                          (org-get-outline-path)
		          (1- (frame-width))
		          nil org-picklink-breadcrumbs-separator)))
	          (if (eq "" s) "" (concat s org-picklink-breadcrumbs-separator)))))))
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (org-picklink--push-link
           target-buffer
           (org-id-get (point) t)
           (or selected-string
               (concat breadcrumbs
                       (org-entry-get (point) "ITEM")))))))))

(defun org-picklink--push-link (target-buffer id description)
  "Internal function of `org-picklink-push-link'."
  (with-current-buffer target-buffer
    (goto-char (gethash :window-point org-picklink-info))
    ;; We should test the origin window-point change or not.
    (if (equal (org-picklink-hash) (gethash :hash org-picklink-info))
        (progn
          ;; When a link is found at point, insert ", "
          (when (save-excursion
                  (let* ((end (point))
                         (begin (line-beginning-position))
                         (string (buffer-substring-no-properties
                                  begin end)))
                    (and (string-match-p "]]$" string)
                         (not (string-match-p ", *$" string)))))
            (insert ", "))
          (org-insert-link nil (format "id:%s" id) description)
          (puthash :window-point (point) org-picklink-info)
          (puthash :links nil org-picklink-info)
          (message "[[id:%s][%s]] -> \"%s\""
                   (concat (substring id 0 6) "...")
                   description target-buffer))
      (push (cons id description) (gethash :links org-picklink-info))
      (message "WARN: please move to proper position and run `org-picklink' again."))))

(defun org-picklink-hash ()
  "Return the sha1 of the content before point."
  (sha1 (buffer-substring-no-properties (point-min) (point))))

;;;###autoload
(defun org-picklink-quit-window ()
  "Quit org agenda window and return org mode window.
Before quit, this command will do some clean jobs."
  (interactive)
  ;; Hide header line in org-agenda window.
  (when org-agenda-buffer
    (with-current-buffer (get-buffer org-agenda-buffer)
      (org-picklink-mode -1)
      (setq header-line-format nil)))
  (org-agenda-quit)
  ;; Update window point in org-mode window
  (when (gethash :buffer org-picklink-info)
    (set-window-point
     (gethash :window org-picklink-info)
     (gethash :window-point org-picklink-info)))
  ;; Clean hashtable `org-picklink-info'
  (puthash :buffer nil org-picklink-info)
  (puthash :window nil org-picklink-info)
  (puthash :window-point nil org-picklink-info))

;;;###autoload
(defun org-picklink-push-link-and-quit-window ()
  "Push link to org mode window and quit org agenda window."
  (interactive)
  (if (gethash :buffer org-picklink-info)
      (progn (call-interactively 'org-picklink-push-link)
             (org-picklink-quit-window))
    (call-interactively 'org-agenda-switch-to)))

;;;###autoload
(defun org-picklink (&optional search-tag)
  "Open org agenda window as a link selector.

if region is actived, ‘org-agenda’ will search string
in region and replace it with selected link.

When SEARCH-TAG is t, use `org-tags-view' instead
of `org-search-view'.

This command only useful in org mode buffer."
  (interactive "P")
  (if (gethash :links org-picklink-info)
      (let ((id (car (car (gethash :links org-picklink-info))))
            (description (cdr (car (gethash :links org-picklink-info)))))
        (org-insert-link nil (format "id:%s" id) description)
        (pop (gethash :links org-picklink-info)))
    (let ((org-agenda-window-setup 'only-window)
          (buffer (current-buffer))
          (search-string
           (if mark-active
               (buffer-substring-no-properties
                (region-beginning) (region-end))
             "")))
      ;; Update `org-picklink-info'
      (if (derived-mode-p 'org-mode)
          (progn
            (puthash :buffer buffer org-picklink-info)
            (puthash :window (get-buffer-window) org-picklink-info)
            (puthash :window-point (point) org-picklink-info)
            (puthash :hash (org-picklink-hash) org-picklink-info))
        (setq org-picklink-info (clrhash org-picklink-info)
              search-string nil))
      ;; Call org-agenda
      (when (and search-string (> (length search-string) 0))
        (delete-region (region-beginning) (region-end))
        (puthash :window-point (point) org-picklink-info))
      (if search-tag
          (org-tags-view nil search-string)
        (org-search-view nil search-string))
      ;; Update `header-line-format'
      (when (derived-mode-p 'org-agenda-mode)
        (with-current-buffer (get-buffer org-agenda-buffer)
          (org-picklink-mode 1)
          (setq header-line-format
                (format
                 (substitute-command-keys
                  (concat
                   "## Type `\\[org-picklink-push-link]' or `\\[org-picklink-push-link-and-quit-window]' "
                   "to push links to buffer \"%s\". ##"))
                 (buffer-name buffer))))))))

(define-minor-mode org-picklink-mode
  "org picklink mode"
  nil " org-picklink")

(provide 'org-picklink)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org-picklink.el ends here
