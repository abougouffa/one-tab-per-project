;;; one-tab-per-project.el --- One tab per project, with unique names -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; URL: https://github.com/abougouffa/one-tab-per-project
;; Version: 1.0.2
;; Package-Requires: ((emacs "28.1") (unique-dir-name "1.0.0"))
;; Keywords: convenience

;;; Commentary:

;; Automatically open/close a tab per project, when two projects have the same
;; name, make it unique.
;; Inspired by `project-tab-groups'

;;; Code:

(require 'seq)
(require 'project)
(require 'unique-dir-name)

(defgroup otpp nil
  "One tab per project."
  :group 'project)

(defcustom otpp-preserve-non-otpp-tab t
  "When non-nil, preserve the current rootless tab when switching projects."
  :group 'otpp
  :type 'boolean)

(defcustom otpp-reconnect-tab t
  "Whether to reconnect a disconnected tab when switching to it.

When set to a function's symbol, that function will be called
with the switched-to project's root directory as its single
argument.

When non-nil, show the project dispatch menu instead."
  :group 'otpp
  :type '(choice function boolean))

(defcustom otpp-strictly-obey-dir-locals nil
  "Whether to strictly obey local variables.

Set a nil (default value) for only respect the variables when the
are defined in the project root.

Set to a function that takes (DIR PROJECT-ROOT DIR-LOCALS-ROOT),
see `otpp--get-project-base-name'.

This can be useful when the project includes sub-projects (a Git
repository with sub-modules, a Git repository with other Git
repos inside, etc)."
  :group 'otpp
  :type '(choice function boolean))

(defcustom otpp-post-change-tab-root-functions nil
  "List of functions to call after changing the `otpp-root-dir' of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir'.
The current tab is supplied as an argument."
  :group 'otpp
  :type 'hook)

;;; Internals and helpers

(defvar otpp--unique-tabs-map (make-hash-table :test 'equal))

(defun otpp--update-all-tabs ()
  "Update all the unique tab names from the root directories."
  (dolist (tab (funcall tab-bar-tabs-function))
    (when-let* ((path (alist-get 'otpp-root-dir tab))
                (unique (gethash path otpp--unique-tabs-map)))
      (let ((explicit-name (assoc 'explicit-name tab)))
        ;; Don't update the tab name if it was renamed explicitly using `tab-bar-rename-tab'
        (unless (eq (cdr explicit-name) t)
          (setcdr (assoc 'name tab) (alist-get 'unique-name unique))
          (setcdr explicit-name 'otpp))))) ; Set the `explicit-name' to `otpp'
  (force-mode-line-update))

;;;###autoload(put 'project-name 'safe-local-variable 'stringp)
;;;###autoload(put 'otpp-project-name 'safe-local-variable 'stringp)

(defun otpp--get-project-base-name (dir)
  "Get the project name from DIR.

This extracts the project root and finds a `dir-locals-file' file
that can be applied to the directory DIR, the local variables are
read if any of these conditions is correct:

- `otpp-strictly-obey-dir-locals' is a function, and calling it
  returns non-nil (we pass to this function the DIR, project root
  and the directory containing the `dir-locals-file'.
- `otpp-strictly-obey-dir-locals' is a *not* a function and it is
  non-nil.
- The `dir-locals-file' is stored in the project root, a.k.a.,
  the project root is the same as the `dir-locals-file'
  directory.

Then, this function checks in this order:

1. If the local variable `otpp-project-name' is bound and
   contains a value, use it as project name.
2. Same with the local variable `project-name'.
3. If the function `project-name' is defined, call it on the
   current project."
  (when-let* ((default-directory dir)
              (proj (project-current))
              (root (project-root proj))
              ;; When can find a `dir-locals-file' that can be applied to files inside
              ;; `dir', we do some extra checks to determine if we should take it into
              ;; account or not.
              (dir-locals-root (car (ensure-list (dir-locals-find-file (expand-file-name "dummy-file" dir)))))
              (_ (or (equal (expand-file-name root) (expand-file-name dir-locals-root))
                     (if (functionp otpp-strictly-obey-dir-locals)
                         (funcall otpp-strictly-obey-dir-locals dir root dir-locals-root)
                       otpp-strictly-obey-dir-locals))))
    (hack-dir-local-variables-non-file-buffer)
    (or (bound-and-true-p otpp-project-name)
        (bound-and-true-p project-name)
        (and (fboundp 'project-name) (project-name proj)))))

;;; API

;;;###autoload
(defun otpp-change-tab-root-dir (dir &optional tab-number)
  "Change the `otpp-root-dir' attribute to DIR.
If if the obsolete TAB-NUMBER is provided, set it, otherwise, set the
current tab.
When DIR is empty, delete it from the tab."
  (interactive
   (list (completing-read
          "Root directory for tab (leave blank to remove the tab root directory): "
          (delete-dups
           (delq nil (mapcar (apply-partially #'alist-get 'otpp-root-dir)
                             (funcall tab-bar-tabs-function)))))
         current-prefix-arg))
  (let* ((dir (expand-file-name dir))
         (tabs (funcall tab-bar-tabs-function))
         (index (if tab-number
                    (1- (max 0 (min tab-number (length tabs))))
                  (tab-bar--current-tab-index tabs)))
         (tab (nth index tabs))
         (root-dir (assq 'otpp-root-dir tab))
         (tab-new-root-dir (and (not (string-empty-p dir)) dir)))
    (if root-dir
        (setcdr root-dir tab-new-root-dir)
      (nconc tab `((otpp-root-dir . ,tab-new-root-dir)))
      ;; Register in the unique names hash-table
      (unique-dir-name-register dir :base (otpp--get-project-base-name dir) :map 'otpp--unique-tabs-map))
    (otpp--update-all-tabs) ; Update all tabs
    (run-hook-with-args 'otpp-post-change-tab-root-functions tab)))

(defun otpp-find-tabs-by-root-dir (dir)
  "Return a list of tabs that have DIR as `otpp-root-dir' attribute."
  (seq-filter
   (lambda (tab) (equal (expand-file-name dir) (alist-get 'otpp-root-dir tab)))
   (funcall tab-bar-tabs-function)))

(defun otpp-select-or-create-tab-root-dir (dir)
  "Select or create the tab with root directory DIR.
Returns non-nil if a new tab was created, and nil otherwise."
  (if-let ((tab (car (otpp-find-tabs-by-root-dir dir))))
      (prog1 nil
        (tab-bar-select-tab (1+ (tab-bar--tab-index tab))))
    (tab-bar-new-tab)
    (otpp-change-tab-root-dir dir) ; Set the root directory for the current tab
    t))

;;; Advices for the integration with `project'

(defun otpp--project-current-a (orig-fn &rest args)
  "Call ORIG-FN with ARGS, set the `otpp-root-dir' accordingly.

Does nothing unless the user was allowed to be prompted for a
project if needed (that is, the `maybe-prompt' argument in the
advised function call was non-nil), or if they did not select a
project when prompted.

Does nothing if the current tab belongs to the selected project.

If the current tab does not have an `otpp-root-dir' attribute, and if
the value of `otpp-preserve-non-otpp-tab' is nil, then set the root
directory for the current tab to represent the selected project.

Otherwise, select or create the tab represents the selected project."
  (let* ((proj-curr (apply orig-fn args))
         (maybe-prompt (car args))
         (proj-dir (and proj-curr (project-root proj-curr))))
    (when (and maybe-prompt proj-dir)
      (let ((curr-tab-root-dir (alist-get 'otpp-root-dir (tab-bar--current-tab)))
            (target-proj-root-dir (expand-file-name proj-dir)))
        (unless (equal curr-tab-root-dir target-proj-root-dir)
          (if (or curr-tab-root-dir (otpp-find-tabs-by-root-dir target-proj-root-dir) otpp-preserve-non-otpp-tab)
              (otpp-select-or-create-tab-root-dir target-proj-root-dir)
            (otpp-change-tab-root-dir target-proj-root-dir)))))
    proj-curr))

(defun otpp--project-switch-project-a (orig-fn &rest args)
  "Switch to the selected project's tab if it exists.
Call ORIG-FN with ARGS otherwise."
  (let ((proj-dir (expand-file-name (or (car args) (funcall project-prompter)))))
    (if (otpp-select-or-create-tab-root-dir proj-dir)
        (funcall orig-fn proj-dir)
      (if (not (file-in-directory-p default-directory proj-dir))
          (if (functionp otpp-reconnect-tab)
              (funcall otpp-reconnect-tab proj-dir)
            (when otpp-reconnect-tab
              (funcall orig-fn proj-dir)))))))

(defun otpp--project-kill-buffers-a (orig-fn &rest args)
  "Call ORIG-FN with ARGS, then close the current tab group, if any."
  (when (apply orig-fn args)
    (when-let ((curr-tab-root-dir (alist-get 'otpp-root-dir (tab-bar--current-tab))))
      (tab-bar-close-tab)
      (unique-dir-name-unregister curr-tab-root-dir :map 'otpp--unique-tabs-map)
      (otpp--update-all-tabs))))

;;;###autoload
(define-minor-mode otpp-mode
  "Automatically create a tab per project, name them uniquely."
  :group 'otpp
  :global t
  (dolist (fn '(project-current project-switch-project project-kill-buffers))
    (let ((advice-fn (intern (format "otpp--%s-a" fn))))
      (if otpp-mode
          (advice-add fn :around advice-fn)
        (advice-remove fn advice-fn)))))

;;;###autoload
(defalias 'one-tab-per-project-mode 'otpp-mode)


(provide 'otpp)
(provide 'one-tab-per-project)
;;; one-tab-per-project.el ends here
