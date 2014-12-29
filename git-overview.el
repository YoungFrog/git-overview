;;; git-overview.el --- Overview of your git repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nicolas Richard

;; Author: Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Keywords: git org
;; Package-Requires: ((magit "0") (let-alist "1.0.3"))
;; Version: 20141227.1

;; This file is NOT part of GNU Emacs.

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

;; Get an overview of all your git repos with Org mode.

;;; Code:

(require 'magit)
(require 'let-alist)
(require 'cl-macs)

(defcustom git-overview-repositories 'magit
  "Repositories which you'd like to keep track of.

The default value is the symbol `magit', which means to keep
track of magit repositories.

To customize the list of repositories, you can set this variable
to an alist as described in the documentation of the function
`git-overview-repositories'."
  :group 'git-overview)

(defun git-overview-repositories ()
  "Repositories of which to keep track.
This returns an alist mapping a category (a string) to a list of
git directories, controlled by the option
`git-overview-repositories'.

Example: '((\"My repos\" \"/path/to/myrepo1\" \"myrepo2\")
           (\"Watched repos\" \"/path/to/some/other/repo\")
           (\"Other repos\" \"/path/to/git/repo\"))"
  (if (eq git-overview-repositories 'magit)
      (list (cons "Magit repositories"
                  (mapcar 'cdr (magit-list-repos magit-repo-dirs))))
    git-overview-repositories))

(defun git-overview ()
  "Open an Org mode buffer with an overview of your git repositories.

The list of repositories is taken from `git-overview-repositories'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Git Overview*")
    (let ((inhibit-read-only t)) (erase-buffer))
    (insert "#+COLUMNS: %25ITEM %20Tracks %7Behind %7Ahead\n")
    (git-overview-mode)
    (dolist (category (git-overview-repositories))
      (insert "* " (car category) "\n")
      (dolist (repo (cdr category))
        (git-overview-insert-repo repo 2)))
    (org-update-statistics-cookies t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (pop-to-buffer (current-buffer))))

(defun git-overview-insert-repo (repo level)
  "Insert a level LEVEL Org heading for repository REPO."
  (git-overview-with-repo repo
    (insert (make-string level ?*) " " (file-name-nondirectory (directory-file-name repo)) ": ")
    (insert .branch)
    (insert " [/]\n")
    (org-entry-put (point) "Gitdir" .gitdir)
    (dolist (branch .branches)
      (let-alist branch
        (insert (make-string level ?*) "* " .branch "\n")
        (when .tracking
          (org-entry-put (point) "Tracks" .tracking)
          (when .behind (org-entry-put (point) "Behind" .behind))
          (when .ahead (org-entry-put (point) "Ahead" .ahead))
          (when (or .behind .ahead)
            (org-todo 1)))))))

(defun git-overview-magit-status (&optional arg)
  (interactive "P")
  (let ((dir (org-entry-get (point) "Gitdir" t)))
    (if (and dir (not arg))
        (magit-status dir)
      (call-interactively #'magit-status))))

(define-derived-mode git-overview-mode org-mode "Git overview"
  t)

(define-key git-overview-mode-map [remap magit-status] #'git-overview-magit-status)

(defmacro git-overview-with-repo (repo &rest body)
  "Give access, within BODY, to the information for repo REPO.
The symbols .gitdir, .branch (current branch), .tracking (which
branch the current branch is tracking, if any) and .branches are
defined.

.branches is a list of alists, in which the keys branch, sha1,
tracking, ahead and behind are used."
  (declare (indent 1)
           (debug (form body)))
  `(let-alist (git-overview-info-plist ,repo)
     ,@body))

(defun git-overview-info-plist (&optional repo)
  "Gather information about REPO and return it in an alist."
  (let ((topdir (ignore-errors (magit-get-top-dir repo))))
    (if (not topdir)
        (warn "No git repo found in %s" repo)
      (let ((default-directory (or topdir default-directory)))
        `((branch . ,(magit-get-current-branch))
          (tracking . ,(magit-get-remote/branch))
          (branches . ,(git-overview--branch-info))
          (gitdir . ,topdir))))))

(defconst git-overview--branch-line-re
  ;; mostly stolen from `magit-wash-branch-line-re'
  (concat "^\\([ *] \\)"                ; 1: current branch marker
          "\\(.+?\\) +"                 ; 2: branch name
          "\\(?:"
          "\\([0-9a-fA-F]+\\)"          ; 3: sha1
          " "
          "\\(?:\\["
          "\\([^:\n]+\\)"              ; 4: tracking
          "\\(?:: \\)?"
          "\\(?:[^]]+ \\([0-9]+\\)\\)?"  ; 5: ahead
          "\\(?:,[^]]+ \\([0-9]+\\)\\)?" ; 6: behind
          "\\] \\)?"
          "\\(?:.*\\)"                  ; message
          "\\|"                         ; or
          "-> "                         ; the pointer to
          "\\(.+\\)"                    ; 7: a ref
          "\\)\n")
  "This is for `git-overview--branch-info' to use")

(defun git-overview--branch-info ()
  "For parsing the output of git branch -vv"
  (let ((result))
    (with-temp-buffer
      (cl-assert (= 0 (call-process "git" nil (current-buffer) nil "branch" "-vv")))
      (goto-char (point-min))
      (while (looking-at git-overview--branch-line-re)
        (let* ((marker      (match-string 1))
               (branch      (match-string 2))
               (sha1        (match-string 3))
               (tracking    (match-string 4))
               (ahead       (match-string 5))
               (behind      (match-string 6)))
          (push `((branch . ,branch)
                  (active . ,(equal marker "* "))
                  (hash . ,sha1)
                  (tracking . ,tracking)
                  (ahead . ,ahead)
                  (behind . ,behind))
                result))
        (forward-line 1))
      (cl-assert (eobp))
      result)))

(provide 'git-overview)
;;; git-overview.el ends here
