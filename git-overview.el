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

;; Set up : customize `git-overview-repositories'. If you don't, the
;; default is to use the repositories that magit knows via
;; `magit-repo-dirs'.

;; Usage : M-x git-overview RET

;; Currently this only shows the information with no easy way to act
;; on the repositories.

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
                  (git-overview-magit-list-repos)))
    git-overview-repositories))

(defun git-overview-magit-list-repos ()
  "List of directories considered by magit."
  (if (help-function-arglist 'magit-list-repos)
      ;; older magit had mandatory argument
      (mapcar 'cdr (magit-list-repos (list magit-repo-dirs)))
    ;; newer magit has zero argument
    (magit-list-repos)))

(defun git-overview (level)
  "Open an Org mode buffer with an overview of your git repositories.

The list of repositories is taken from `git-overview-repositories'.

LEVEL is the number of stars for the categories returned by
`git-overview-repositories'."
  (interactive "p")
  (with-current-buffer (get-buffer-create "*Git Overview*")
    (let ((inhibit-read-only t)) (erase-buffer))
    (insert "#+COLUMNS: %25ITEM %20Tracks %7Behind %7Ahead\n")
    (git-overview-mode)
    (dolist (category (git-overview-repositories))
      (insert (make-string level ?*) " " (car category) "\n")
      (dolist (repo (cdr category))
        (git-overview-insert-repo repo (org-get-valid-level level 1))))
    (org-update-statistics-cookies t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (pop-to-buffer (current-buffer))))

(defun git-overview-insert-repo (repo level)
  "Insert a level LEVEL Org heading for repository REPO."
  (insert (make-string level ?*)
          " "
          (file-name-nondirectory (directory-file-name repo))
          " [/]\n")
  (org-entry-put (point) "Gitdir" repo)
  (git-overview-update-repo)
  (save-restriction
    (git-overview-narrow-to-repo)
    (goto-char (point-max))))

(defun git-overview-insert-branch (branch-info level)
  (insert (make-string level ?*) " ")
  (insert (cdr (assoc 'branch branch-info)))
  (insert "\n")
  (save-excursion
    (git-overview-update-branch branch-info)
    (org-end-of-subtree t)))


(defun git-overview-branch-at-point ()
  (org-entry-get (point) "Branch"))
(defun git-overview-repo-at-point ()
  (org-entry-get (point) "Gitdir" t))

(defun git-overview-narrow-to-repo ()
  "Narrow to the repo at point, and return its location."
  (let ((repo (git-overview-repo-at-point)))
    (when repo
      (if (marker-buffer org-entry-property-inherited-from)
          (goto-char org-entry-property-inherited-from))
      (org-narrow-to-subtree)
      (cl-assert (equal repo (git-overview-repo-at-point)))
      repo)))

(defun git-overview-update-repo ()
  (interactive)
  (save-excursion
    (save-restriction
      (git-overview-with-repo (git-overview-narrow-to-repo)
        (goto-char (point-min))
        (while (outline-next-heading) ;; we're going to visit every single subheading.
          ;; Check if this is a subtree with branch information
          (-when-let (branch-at-point (git-overview-branch-at-point))
            ;; check if it exists or not.
            (--if-let (cl-member
                       branch-at-point
                       .branches
                       :test
                       (lambda (x y)
                         (let-alist y
                           (equal .branch x))))
                (progn
                  (git-overview-update-branch (car it))
                  (callf2 delq (car it) .branches))
              nil
              ;; this is the "this looks like a branch but it
              ;; doesn't exist" case. remove it seems a bit too much...
              )))
        (goto-char (point-max))
        (dolist (branch .branches)
          (org-back-over-empty-lines)
          (git-overview-insert-branch branch
                                      (org-get-valid-level
                                       (save-excursion (goto-char (point-min))
                                                       (org-outline-level))
                                       1)))))))

(defun git-overview-update-branch (branch-info)
  "Update branch at point using data from BRANCH-INFO.
The argument should be an alist."
  (let-alist branch-info
    (dolist (i `(("Tracks" , .tracking)
                 ("Behind" , .behind)
                 ("Ahead"  , .ahead)
                 ("Branch"  , .branch)))
      (git-overview--update-entry-at-point (car i) (cadr i)))
    (if (or .behind .ahead)
        (org-todo 1)
      (org-todo 'none))))

(defun git-overview--update-entry-at-point (key value)
  "Remove or update property KEY to VALUE.
When VALUE is nil, the property is instead removed."
  (if value
      (org-entry-put (point) key value)
    (org-entry-delete (point) key)))

(defun git-overview-magit-status (&optional arg)
  (interactive "P")
  (let ((dir (git-overview-repo-at-point)))
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
          (tracking . ,(if (fboundp 'magit-get-remote-branch)
                           (let ((it (magit-get-remote-branch)))
                             (format "%s/%s" (car it) (cdr it)))
                         (magit-get-remote/branch)))
          (branches . ,(git-overview--branch-info))
          (gitdir . ,topdir))))))

(defconst git-overview--branch-line-re
  ;; mostly stolen from `magit-wash-branch-line-re'
  (concat "^"
          "\\(?1:[ \\*]\\) "
          "\\(?2:([^)]+)\\|[^ ]+?\\)"   ; branch
          "\\(?: +\\)"
          "\\(?3:[0-9a-fA-F]+\\) "      ; sha1
          "\\(?:\\["
          "\\(?5:[^:]+\\): "            ; upstream
          "\\(?:"
          "\\(?8:gone\\)\\|"               ; gone
          "\\(?:ahead \\(?6:[0-9]+\\)\\)?" ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?7:[0-9]+\\)\\)?" ; behind
          "\\)"
          "\\] \\)?"
          "\\(?4:.*\\)")
  "This is for `git-overview--branch-info' to use")

(defun git-overview--branch-info ()
  "For parsing the output of git branch -vv"
  (let ((result))
    (with-temp-buffer
      (let ((process-environment (cons '("LANG" . "C")
                                 process-environment)))
        (cl-assert (= 0 (call-process "git" nil (current-buffer) nil "branch" "-vv"))))
      (goto-char (point-min))
      (while (looking-at git-overview--branch-line-re)
        (let* ((marker      (match-string 1))
               (branch      (match-string 2))
               (sha1        (match-string 3))
               (tracking    (match-string 5))
               (ahead       (match-string 6))
               (behind      (match-string 7))
               (gone        (match-string 8)))
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
