;;; one-tab-per-project.el --- One tab per project, with unique names -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; URL: https://github.com/abougouffa/one-tab-per-project
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (unique-dir-name "0.0.1"))
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
  :type 'boolean
  :group 'otpp)

(defcustom otpp-reconnect-tab t
  "Whether to reconnect a disconnected tab when switching to it.

When set to a function's symbol, that function will be called
with the switched-to project's root directory as its single
argument.

When non-nil, show the project dispatch menu instead."
  :type '(choice function boolean)
  :group 'otpp)

(defcustom tab-bar-tab-post-change-group-functions nil
  "List of functions to call after changing the `otpp-root-dir' of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir'.
The current tab is supplied as an argument."
  :type 'hook
  :group 'otpp)

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

(defun otpp-change-tab-root-dir (dir &optional tab-number)
  "Change the `otpp-root-dir' attribute to DIR.
If if the absolete TAB-NUMBER is provided, set it, otherwise, set the
current tab."
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
      (unique-dir-name-register dir :map 'otpp--unique-tabs-map))
    (otpp--update-all-tabs) ; Update all tabs
    (run-hook-with-args 'otpp-post-change-tab-root-functions tab)))

(defun otpp--find-tab-by-root-dir (dir)
  "Find the first tab that have DIR as `otpp-root-dir' attribute."
  (seq-find
   (lambda (tab) (equal (expand-file-name dir) (alist-get 'otpp-root-dir tab)))
   (funcall tab-bar-tabs-function)))

(defun otpp--select-or-create-tab-root-dir (dir)
  "Select or create the tab with root directory DIR.
Returns non-nil if a new tab was created, and nil otherwise."
  (if-let ((tab (otpp--find-tab-by-root-dir dir)))
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
adviced function call was non-nil), or if they did not select a
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
          (if (or curr-tab-root-dir (otpp--find-tab-by-root-dir target-proj-root-dir) otpp-preserve-non-otpp-tab)
              (otpp--select-or-create-tab-root-dir target-proj-root-dir)
            (otpp-change-tab-root-dir target-proj-root-dir)))))
    proj-curr))

(defun otpp--project-switch-project-a (orig-fn &rest args)
  "Switch to the selected project's tab if it exists.
Call ORIG-FN with ARGS otherwise."
  (let ((proj-dir (expand-file-name (or (car args) (funcall project-prompter)))))
    (if (otpp--select-or-create-tab-root-dir proj-dir)
        (funcall orig-fn proj-dir)
      (if (not (file-in-directory-p default-directory proj-dir))
          (if (functionp otpp-reconnect-tab)
              (funcall otpp-reconnect-tab proj-dir)
            (when otpp-reconnect-tab
              (funcall orig-fn proj-dir)))))))

(defun otpp--project-kill-buffers-a (orig-fn &rest args)
  "Call ORIG-FN with ARGS, then close the current tab group, if any."
  (when (apply orig-fn args)
    (when-let* ((curr-tab (tab-bar--current-tab))
                (root-dir (alist-get 'otpp-root-dir curr-tab)))
      (tab-bar-close-tab)
      (unique-dir-name-unregister root-dir :map 'otpp--unique-tabs-map)
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
