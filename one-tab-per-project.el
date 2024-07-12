;;; one-tab-per-project.el --- One tab per project, with unique names -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; URL: https://github.com/abougouffa/one-tab-per-project
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1") (unique-dir-name "1.0.0"))
;; Keywords: convenience

;;; Commentary:

;; This is a lightweight workspace management package that provides a thin layer
;; between builtin packages `project' and `tab-bar'. The whole idea consists of
;; creating a tab per opened project while ensuring unique names for the created
;; tabs (when multiple opened projects have the same name).
;;
;; This package is inspired by `project-tab-groups' which creates a "tab group"
;; per project.

;;; Installation

;; This package is not yet on MELPA, you need to installed from the GitHub
;; repository.
;;
;; ```emacs-lisp
;; ;; `one-tab-per-project' depends on `unique-dir-name', which is not on MELPA
;; (use-package unique-dir-name
;;   :straight (:host github :repo "abougouffa/unique-dir-name"))
;;
;; (use-package one-tab-per-project
;;   :straight (:host github :repo "abougouffa/one-tab-per-project")
;;   :after project
;;   :init
;;   (otpp-mode 1))
;; ```

;;; Usage

;; The usage is quite straightforward, there is no extra commands to learn to be
;; able to use it. When `otpp-mode' global minor mode is enabled, you will have
;; this:
;;
;; - When you switch to a project `project-switch-project' (bound by default to
;;   `C-x p p'), `otpp' will create a tab with the project name.
;;
;; - When you kill a project with all its buffers with `project-kill-buffers', the
;;   tab is closed.
;;
;; - Lets say you've switched to the project under
;;   `/home/user/project1/backend/', `otpp' will create a tab named `backend'
;;   for this particular project. Now, you opened a second project under
;;   `/home/user/project2/backend/', `otpp' will detect that the name of the
;;   project `backend' is the same as the previously opened one, but it have a
;;   different path. In this case, `otpp' will create a tab named
;;   `backend[project2]' and renames the previously opened tab to
;;   `backend[project1]'. This conflict resolution is provided by the
;;   [`unique-dir-name'](https://github.com/abougouffa/unique-dir-name) library,
;;   which works like the built-in `uniquify' library used to keep distinct
;;   names for buffer names.
;;
;; - For some cases, you might need to attach a manually created tab (by
;;   `tab-bar-new-tab') to an opened project so you have two tabs dedicated to
;;   the same project (with different windows layouts for example). In this
;;   case, you can call the command `otpp-change-tab-root-dir' and select the
;;   path of the project to attach to.

;;; Similar packages

;; This section is not exhaustive, it includes only the packages that I used
;; before.
;;
;; - [`project-tab-groups'](https://github.com/fritzgrabo/project-tab-groups):
;;   This package provides a mode that enhances the Emacs built-in `project' to
;;   support keeping projects isolated in named tab groups. `otpp' is inspired
;;   by this package, but instead of setting the tab groups, `otpp' introduces a
;;   new attribute in the tab named `otpp-root-dir' where it stores the root
;;   directory of the project bound to the tab. This allows keeping the tabs
;;   updated in case another project with the same name (but a different path)
;;   is opened.
;;
;; - [`tabspaces'](https://github.com/mclear-tools/tabspaces): This package
;;   provide workspace management with `tab-bar' and with an integration with
;;   `project'. Contrary to `otpp' and `project-tab-groups', `tabspaces' don't
;;   create tabs automatically, you need to call specific commands like
;;   `tabspaces-open-or-create-project-and-workspace'.

;;; Code:

(require 'seq)
(require 'project)
(require 'unique-dir-name)

(defgroup otpp nil
  "One tab per project."
  :group 'project)

(defcustom otpp-preserve-non-otpp-tabs t
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

Set a nil (default value) to only respect the local variables when they
are defined in the project's root (the `dir-locals-file' is located in
the project's root).

Set to a function that takes `(DIR PROJECT-ROOT DIR-LOCALS-ROOT)'
as argument, see the function `otpp-project-name'. The function
should return non-nil to take the local variables into account.

This can be useful when the project include sub-projects (a Git
repository with sub-modules, a Git repository with other Git repos
inside, a Repo workspace, etc)."
  :group 'otpp
  :type '(choice function boolean))

(defcustom otpp-post-change-tab-root-functions nil
  "List of functions to call after changing the `otpp-root-dir' of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir'.
The current tab is supplied as an argument."
  :group 'otpp
  :type 'hook)

(defcustom otpp-project-name-function #'otpp-project-name
  "Derrive project name from a directory.

This function receives a directory and return the project name
for the project that includes this path."
  :group 'otpp
  :type '(choice function (symbol nil)))


;;; Internals and helpers

(defvar-local otpp-project-name nil)
(unless (boundp 'project-vc-name) ; Should be present in Emacs 29.1, Project 0.9.0
  (defvar-local project-vc-name nil))

;;;###autoload(put 'project-vc-name 'safe-local-variable 'stringp)
;;;###autoload(put 'otpp-project-name 'safe-local-variable 'stringp)

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

;;; API

(defun otpp-project-name (dir)
  "Get the project name from DIR.

This function extracts the project root. Then, it tries to find a
`dir-locals-file' file that can be applied to files inside the directory
DIR. When found, the local variables are read if any of these conditions
is correct:

- `otpp-strictly-obey-dir-locals' is set to a function, and calling it
  returns non-nil (we pass to this function the DIR, the project root
  and the directory containing the `dir-locals-file').
- `otpp-strictly-obey-dir-locals' is a *not* a function and it is
  non-nil.
- The `dir-locals-file' file is stored in the project root, a.k.a.,
  the project root is the same as the `dir-locals-file' directory.

Then, this function checks in this order:

1. If the local variable `otpp-project-name' is set locally in the
`dir-locals-file', use it as project name.
2. Same with the local variable `project-vc-name'.
3. If the function `project-name' is defined (Emacs 29.1 / Project
   0.9.0), call it on the current project.
4. Return the directory name of the project's root.

When DIR isn't part of any project, returns nil."
  (when-let* ((dir (expand-file-name dir))
              (proj (project-current nil dir))
              (root (project-root proj)))
    ;; When can find a `dir-locals-file' that can be applied to files inside
    ;; `dir', we do some extra checks to determine if we should take it into
    ;; account or not.
    (with-temp-buffer
      (setq default-directory dir)
      (let (project-vc-name otpp-project-name) ; BUG: Force them to nil to ensure we are using the local values
        (when-let* ((dir-locals-root (car (ensure-list (dir-locals-find-file (expand-file-name "dummy-file" dir)))))
                    (_ (or (equal (expand-file-name root) (expand-file-name dir-locals-root))
                           (if (functionp otpp-strictly-obey-dir-locals)
                               (funcall otpp-strictly-obey-dir-locals dir root dir-locals-root)
                             otpp-strictly-obey-dir-locals))))
          (hack-dir-local-variables-non-file-buffer))
        (or otpp-project-name
            project-vc-name ; BUG: Don't use `project-name' function as it's behaving strangly for nested projects
            (file-name-nondirectory (directory-file-name root)))))))

;;;###autoload
(defun otpp-change-tab-root-dir (dir &optional tab-number)
  "Change the `otpp-root-dir' attribute to DIR.
If if the obsolete TAB-NUMBER is provided, set it, otherwise, set the
current tab.
When DIR is empty or nil, delete it from the tab."
  (interactive
   (list (completing-read
          "Root directory for tab (leave blank to remove the tab root directory): "
          (delete-dups
           (delq nil (mapcar (apply-partially #'alist-get 'otpp-root-dir)
                             (funcall tab-bar-tabs-function)))))
         current-prefix-arg))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (index (if tab-number
                    (1- (max 0 (min tab-number (length tabs))))
                  (tab-bar--current-tab-index tabs)))
         (tab (nth index tabs))
         (root-dir (assq 'otpp-root-dir tab))
         (tab-new-root-dir (and dir (not (string-empty-p dir)) (expand-file-name dir))))
    (if root-dir
        (setcdr root-dir tab-new-root-dir)
      (nconc tab `((otpp-root-dir . ,tab-new-root-dir)))
      ;; Register in the unique names hash-table
      (unique-dir-name-register tab-new-root-dir
                                :base (and otpp-project-name-function
                                           (funcall otpp-project-name-function tab-new-root-dir))
                                :map 'otpp--unique-tabs-map))
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
the value of `otpp-preserve-non-otpp-tabs' is nil, then set the root
directory for the current tab to represent the selected project.

Otherwise, select or create the tab represents the selected project."
  (let* ((proj-curr (apply orig-fn args))
         (maybe-prompt (car args))
         (proj-dir (and proj-curr (project-root proj-curr))))
    (when (and maybe-prompt proj-dir)
      (let ((curr-tab-root-dir (alist-get 'otpp-root-dir (tab-bar--current-tab)))
            (target-proj-root-dir (expand-file-name proj-dir)))
        (unless (equal curr-tab-root-dir target-proj-root-dir)
          (if (or curr-tab-root-dir (otpp-find-tabs-by-root-dir target-proj-root-dir) otpp-preserve-non-otpp-tabs)
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
    (when-let* ((tabs (funcall tab-bar-tabs-function))
                (curr-tab (assq 'current-tab tabs))
                (curr-tab-root-dir (alist-get 'otpp-root-dir curr-tab)))
      (if (length> tabs 1)
          (tab-bar-close-tab)
        ;; When the tab cannot be removed (last tab), remove the association
        ;; with the current project and rename it to the default
        (otpp-change-tab-root-dir nil)
        (setcdr (assq 'name curr-tab) "*default*")
        (setcdr (assq 'explicit-name curr-tab) 'def))
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
