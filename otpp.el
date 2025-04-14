;;; otpp.el --- One tab per project, with unique names -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; URL: https://github.com/abougouffa/one-tab-per-project
;; Created: July 07, 2024
;; Modified: April 14, 2025
;; Version: 3.2.0
;; Package-Requires: ((emacs "28.1") (compat "29.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;; This is a lightweight workspace management package that provides a thin layer
;; between builtin packages `project' and `tab-bar'. The whole idea consists of
;; creating a _tab per opened project_ while ensuring unique names for the
;; created tabs (when multiple opened projects have the same name).
;;
;; This package is inspired by `project-tab-groups' which creates a "tab group"
;; per project.

;;; Installation

;; This package is available on MELPA.
;;
;; ```emacs-lisp
;; (use-package otpp
;;   :straight t
;;   :after project
;;   :init
;;   ;; If you like to define some aliases for better user experience
;;   (defalias 'one-tab-per-project-mode 'otpp-mode)
;;   (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
;;   ;; Enable `otpp-mode' globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands'
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))
;; ```

;;; Basic usage

;; The usage is quite straightforward, there is no extra commands to learn to be
;; able to use it. When `otpp-mode' global minor mode is enabled, you will have
;; this:
;;
;; - When you switch to a project `project-switch-project' (bound by default to
;;   `C-x p p`), `otpp' will create a tab with the project name.
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
;;   `backend[project2]` and renames the previously opened tab to
;;   `backend[project1]`. This conflict resolution is provided by the
;;   `otpp-uniq-*' routines.
;;
;; - For some cases, you might need to attach a manually created tab (by
;;   `tab-bar-new-tab') to an opened project so you have two tabs dedicated to
;;   the same project (with different windows layouts for example). In this
;;   case, you can call the command `otpp-change-tab-root-dir' and select the
;;   path of the project to attach to.
;;
;; - When you use some commands to jump to a file (`find-file',
;;   `xref-find-definitions', etc.), you can end up with a buffer belonging to a
;;   _different project (lets say `B')_ but displayed in the current project's
;;   tab _(`A')_. In this case, you can call `otpp-detach-buffer-to-tab' to
;;   create a new tab dedicated to the buffer's project `B'. When the opened
;;   buffer is project-less (not part of a project), the command will signal a
;;   user error unless `otpp-allow-detach-projectless-buffer' is non-nil, in
;;   this case, `otpp' creates a new project-less tab for the buffer.

;;; Advanced usage

;; Consider this use case: supposing you are using `otpp-mode' and you've run
;; `project-switch-project' to open the `X' project in a new `X' tab. Now you
;; `M-x find-file` then you open the `test.cpp` file outside the current `X'
;; project. Now, if you run `project-find-file', you will be in one of these two
;; situations:
;;
;; 1. If `test.cpp` is part of another project `Y', the `project-find-file' will
;;    prompt you with a list of `Y's files even though we are in the `X' tab.
;;
;; 2. If `test.cpp` isn't part of any project, `project-find-file' will prompt
;; you to select a project first, then to select a file.
;;
;; For this, `otpp' provides `otpp-prefix' (we recommend to bind it to some key,
;; like `C-x t P`, using `otpp-prefix' from `M-x' can have some limitations).
;; When you run `otpp-prefix' followed by `C-x p f` for example, you will be
;; prompted for files in the current's tab project files even if you are
;; visiting a file outside of the current project.
;;
;; In my workflow, I would like to always restrict the commands like
;; `project-find-file' and `project-kill-buffers' to the project bound to the
;; current tab, even if I'm visiting a file which is not part of this project.
;; If you like this behavior, you can enable the `otpp-override-mode'. This mode
;; will advice all the commands defined in `otpp-override-commands' to be ran in
;; the current's tab root directory (_a.k.a._, in the project bound to the
;; current tab).
;;
;; When `otpp-override-mode' is enabled, the `otpp-prefix' acts inversely. While
;; all `otpp-override-commands' are restricted to the current's tab project by
;; default, running a command with `otpp-prefix' will disable this behavior,
;; which results of the next command to be run in the `default-directory'
;; depending on the visited buffer.

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
;;   `tabspaces-open-or-create-project-and-workspace'. Also, `tabspaces'
;;   behavior isn't predictable when you open several projects with the same
;;   directory name.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'project)

(defgroup otpp nil
  "One tab per project."
  :group 'project
  :prefix "otpp-")

(defcustom otpp-preserve-non-otpp-tabs t
  "When non-nil, preserve the current rootless tab when switching projects."
  :group 'otpp
  :type 'boolean
  :version "0.1.0")

(defcustom otpp-bury-on-kill-buffer-when-multiple-tabs t
  "Bury the current buffer when killed but it is opened in another tab.

When non-nil, this modifies the behavior of `kill-buffer' when killing
the current buffer. If the current buffer is opened in another tab, we
bury it instead of killing it. This only affects the current buffer,
when we explicitly select another buffer to kill, `otpp' assumes that we
have a good reason to kill it."
  :group 'otpp
  :type 'boolean
  :version "3.1.0")

(defcustom otpp-reconnect-tab t
  "Whether to reconnect a disconnected tab when switching to it.

When set to a function's symbol, that function will be called
with the switched-to project's root directory as its single
argument.

When non-nil, show the project dispatch menu instead."
  :group 'otpp
  :type '(choice function boolean)
  :version "0.1.0")

(defcustom otpp-strictly-obey-dir-locals nil
  "Whether to strictly obey local variables.

Set a nil (default value) to only respect the local variables when they
are defined in the project's root (the `dir-locals-file' is located in
the project's root).

Set to a function that takes DIR, PROJECT-ROOT and DIR-LOCALS-ROOT as
arguments in this order, see the function `otpp-project-name'. The
function should return non-nil to take the local variables into account.

This can be useful when the project include sub-projects (a Git
repository with sub-modules, a Git repository with other Git repos
inside, a Repo workspace, etc)."
  :group 'otpp
  :type '(choice function boolean)
  :version "1.0.1")

(defcustom otpp-post-change-tab-root-functions nil
  "List of functions to call after changing the `otpp-root-dir' of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir'.
The current tab is supplied as an argument."
  :group 'otpp
  :type 'hook
  :version "1.0.1")

(defcustom otpp-project-name-function #'otpp-project-name
  "Derive project name from a directory.

This function receives a directory and return the project name
for the project that includes this path."
  :group 'otpp
  :type '(choice function (symbol nil))
  :version "1.1.0")

(defcustom otpp-allow-detach-projectless-buffer nil
  "Allow detaching a buffer to a new tab even if it is not part of a project.
This can also be set to a function that receives the buffer, and return
non-nil if we should allow the tab creation."
  :type '(choice boolean function)
  :group 'otpp
  :version "2.0.0")

(defcustom otpp-override-commands
  '(;; project
    project-find-file project-find-dir project-kill-buffers project-switch-to-buffer
    project-shell project-eshell project-dired project-compile
    project-find-regexp project-query-replace-regexp
    ;; consult
    consult-grep consult-find consult-fd consult-ripgrep
    ;; rg
    rg-project
    ;; magit
    magit-project-status
    ;; projection-multi
    projection-multi-compile projection-multi-projection
    ;; projection-dape
    projection-dape)
  "A list of commands to be advised in `otpp-override-mode'.
These commands will be run with `default-directory' set the to current's
tab directory."
  :type '(repeat function)
  :group 'otpp
  :version "1.2.0")

(defcustom otpp-default-tab-name "*default*"
  "The default tab name to use when the last otpp tab is killed."
  :type '(choice string function)
  :group 'otpp)

(defcustom otpp-project-aware-commands-regexp "^project\\(?:ion\\)?-"
  "A regular expression to detect project-aware commands in `otpp-prefix'."
  :type 'regexp
  :group 'otpp
  :version "3.2.0")

(defvar otpp-verbose nil)
(defvar-local otpp-project-name nil)
(defvar otpp-uniq-map-default (make-hash-table :test 'equal))
(defvar otpp-uniq-format "%s[%s]")

;;;###autoload
(put 'otpp-project-name 'safe-local-variable 'stringp)
;;;###autoload
(put 'otpp-project-name 'permanent-local-hook t)

;;; Helpers for generating unique projects names from directories

(defun otpp-uniq--get-dir-elements (dir)
  "Get elements for the DIR path."
  (butlast (reverse (file-name-split (directory-file-name (expand-file-name dir))))))

(defun otpp-uniq--unique-elements (dir1 dir2 &optional base1 base2)
  "Return unique elements of DIR1 and DIR2.
Consider custom base names BASE1 and BASE2 when non-nil."
  (let* ((els1 (otpp-uniq--get-dir-elements dir1))
         (els2 (otpp-uniq--get-dir-elements dir2)))
    ;; Consider custom base names only when they aren't the same as the directory base
    (when (and base1 (not (equal base1 (car els1))))
      (push base1 els1))
    (when (and base2 (not (equal base2 (car els2))))
      (push base2 els2))
    (while-let ((el1 (car els1))
                (el2 (car els2))
                ((string= el1 el2)))
      (pop els1) (pop els2))
    (cons els1 els2)))

(cl-defun otpp-uniq--create-or-update (dir &key base rename-fn (map 'otpp-uniq-map-default))
  "Create or update a unique element for DIR.
For the meaning of :MAP, :RENAME-FN and :BASE, see `otpp-uniq-register'."
  (let* ((dir (expand-file-name dir))
         (dir-name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
         (unique-map (eval map))
         (curr-element (gethash dir unique-map))
         (base (or base (alist-get 'base-name curr-element)))
         (name (or base dir-name))
         (unique-name
          (cl-loop for other-path in (hash-table-keys unique-map)
                   with len-min = most-positive-fixnum
                   with len-max = most-negative-fixnum
                   with max-path = nil
                   do
                   (let ((other-element (gethash other-path unique-map)))
                     (when (and (not (string= dir other-path)) ; not the same dir
                                (string= name (or (alist-get 'base-name other-element)
                                                  (alist-get 'dir-name other-element))))
                       (let ((dir-els (car (otpp-uniq--unique-elements dir other-path base (alist-get 'base-name other-element)))))
                         (let ((len (length dir-els)))
                           (setq len-min (min len-min len))
                           (when (> len len-max)
                             (setq len-max len
                                   max-path dir-els))))))
                   finally return
                   (let ((s (string-join
                             (reverse (butlast max-path
                                               (- (length max-path)
                                                  (1+ (- len-max len-min)))))
                             "/")))
                     (if (string-empty-p s)
                         name
                       (format otpp-uniq-format name s))))))
    (if curr-element
        (let* ((old-unique-name (assq 'unique-name curr-element))
               (old-base (assq 'base-name curr-element)))
          (when (and (functionp rename-fn) (not (equal (cdr old-unique-name) unique-name)))
            (ignore-errors (funcall rename-fn (cdr old-unique-name) unique-name)))
          (setcdr old-unique-name unique-name)
          (unless (equal (cdr old-base) base)
            (setcdr old-base base))
          nil) ; when the element already exist, update it and return nil
      (puthash dir `((dir-name . ,dir-name) (base-name . ,base) (unique-name . ,unique-name)) unique-map)
      t))) ; return t on newly created elements

(cl-defun otpp-uniq-update-all (&key rename-fn (map 'otpp-uniq-map-default))
  "Update all unique names.
This function can be called after manually modifying the hash table used
to keep track of the unique names.
For the meaning of :MAP and :RENAME-FN, see `otpp-uniq-register'."
  (let ((unique-map (eval map)))
    (dolist (path (hash-table-keys unique-map)) ; Update all the names
      (otpp-uniq--create-or-update path :map map :rename-fn rename-fn))))

;;;###autoload
(cl-defun otpp-uniq-register (dir &key base rename-fn (map 'otpp-uniq-map-default))
  "Make a unique name derived from DIR.
If the :BASE string is provided, it will be used as a basis for the
unique name, otherwise, this will be calculated from the directory name
of DIR.
The :MAP is a symbol for the hash-table used to register the names, all
names will be renamed accordingly when needed.
The :RENAME-FN is a function of signature (OLD NEW), called before renaming
the hash-table elements."
  (append
   `((path . ,dir))
   (if-let* ((dir (expand-file-name dir))
             (dir-name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
             (name (or base dir-name))
             (unique-map (eval map))
             (element (gethash dir unique-map)))
       element
     (puthash dir `((dir-name . ,dir-name) (base-name . ,base) (unique-name . ,name)) unique-map)
     (otpp-uniq-update-all :map map :rename-fn rename-fn)
     (gethash dir unique-map))))

;;;###autoload
(cl-defun otpp-uniq-unregister (dir &key rename-fn (map 'otpp-uniq-map-default))
  "Unregister a unique name derived from DIR.
For the meaning of :MAP and :RENAME-FN, see `otpp-uniq-register'."
  (let* ((dir (expand-file-name dir))
         (unique-map (eval map)))
    (remhash dir unique-map)
    (otpp-uniq-update-all :map map :rename-fn rename-fn)
    unique-map))



;;; Internals and helpers

(defvar otpp--unique-tabs-map (make-hash-table :test 'equal))

(defun otpp--update-all-tabs ()
  "Update all the unique tab names from the root directories."
  (otpp--cleanup-unique-map)
  (let ((all-tabs (funcall tab-bar-tabs-function)))
    (dolist (tab all-tabs)
      (when-let* ((path (otpp-get-tab-root-dir tab))
                  (unique (gethash path otpp--unique-tabs-map)))
        (let ((explicit-name (assoc 'explicit-name tab)))
          ;; Don't update the tab name if it was renamed explicitly using `tab-bar-rename-tab'
          (unless (eq (cdr explicit-name) t)
            ;; In case a tab has been created with `otpp-change-tab-root-dir',
            ;; it will have the same root as another tab and the same entry in
            ;; the unique map. We get the index of all similar tabs and we add a
            ;; numeric suffix to them in the same order.
            (let* ((same-path-tabs-indexes
                    (mapcar 'tab-bar--tab-index
                            (seq-filter (lambda (tb)
                                          (when-let* ((p (otpp-get-tab-root-dir tb)))
                                            (equal path p)))
                                        all-tabs)))
                   (name-suffix (and (length> same-path-tabs-indexes 1)
                                     (format "<%d>" (1+ (seq-position same-path-tabs-indexes (tab-bar--tab-index tab)))))))
              (setcdr (assoc 'name tab) (concat (alist-get 'unique-name unique) name-suffix)))
            ;; Set the `explicit-name' field to `otpp'. This tells us that we
            ;; are allowed to modify it it next time we update the names, while
            ;; distinguishing the explicitly named tabs using
            ;; `tab-bar-rename-tab'.
            (setcdr explicit-name 'otpp))))))
  (force-mode-line-update))

(defun otpp--cleanup-unique-map ()
  "Cleanup the unique names hash-table."
  (mapc (lambda (dir) (remhash dir otpp--unique-tabs-map))
        (seq-filter
         (lambda (dir)
           (not (cl-some
                 (lambda (tab) (equal (expand-file-name dir) (otpp-get-tab-root-dir tab)))
                 (funcall tab-bar-tabs-function))))
         (hash-table-keys otpp--unique-tabs-map)))
  (otpp-uniq-update-all :map 'otpp--unique-tabs-map))

(defun otpp--apply-interactively (func &optional args)
  "Apply FUNC to ARGS interactively."
  (apply #'funcall-interactively (cons func args)))

(defvar otpp-run-command-in-tab-root-dir nil)

(defun otpp--call-command-in-root-dir-maybe (cmd &rest args)
  "Run CMD with ARGS in `otpp-root-dir' dep. on `otpp-run-command-in-tab-root-dir'."
  (let ((default-directory (or (and otpp-run-command-in-tab-root-dir (otpp-get-tab-root-dir))
                               default-directory)))
    (otpp--apply-interactively cmd args)))

;;; API

(defun otpp-get-tab-root-dir (&optional tab)
  "Get the root directory set to the TAB, default to the current tab."
  (alist-get 'otpp-root-dir (or tab (tab-bar--current-tab))))

(define-obsolete-function-alias 'otpp-current-tab-root-dir #'otpp-get-tab-root-dir "3.0.3")

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
3. Return the directory name of the project's root.

When DIR isn't part of any project, returns nil."
  (when-let* ((dir (expand-file-name dir))
              (proj (project-current nil dir))
              (root (project-root proj)))
    ;; When can find a `dir-locals-file' that can be applied to files inside
    ;; `dir', we do some extra checks to determine if we should take it into
    ;; account or not.
    (with-temp-buffer
      (setq default-directory dir)
      (when-let* ((dir-locals-root (car (ensure-list (dir-locals-find-file (expand-file-name "dummy-file" dir)))))
                  ((or (equal (expand-file-name root) (expand-file-name dir-locals-root))
                       (if (functionp otpp-strictly-obey-dir-locals)
                           (funcall otpp-strictly-obey-dir-locals dir root dir-locals-root)
                         otpp-strictly-obey-dir-locals))))
        ;; NOTE: Read the local variables, but don't apply them.
        (hack-dir-local-variables))
      (or (cl-some (lambda (var)
                     (or (alist-get var dir-local-variables-alist)
                         (alist-get var file-local-variables-alist)))
                   '(otpp-project-name project-vc-name))
          (file-name-nondirectory (directory-file-name root))))))

;;;###autoload
(defun otpp-detach-buffer-to-tab (buffer)
  "Create or switch to the tab corresponding to the project of BUFFER.
When called with the a prefix, it asks for the buffer."
  (interactive (list (if current-prefix-arg (read-buffer "Select the buffer (leave empty for an unnamed buffer): ") (current-buffer))))
  (with-current-buffer buffer
    (if-let ((proj (project-current))
             (proj-root (project-root proj))
             (this-buff (current-buffer)))
        (progn
          (bury-buffer)
          (otpp-select-or-create-tab-root-dir proj-root)
          (switch-to-buffer this-buff))
      (if (or (and (functionp otpp-allow-detach-projectless-buffer)
                   (funcall otpp-allow-detach-projectless-buffer this-buff))
              otpp-allow-detach-projectless-buffer)
          (let* ((recent-tabs (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar--tabs-recent)))
                 (tab-name (completing-read "Switch to tab by name (leave empty to create an unnamed tab): " recent-tabs)))
            (bury-buffer)
            (if (string-empty-p tab-name)
                (tab-bar-new-tab)
              (tab-bar-select-tab-by-name tab-name))
            (switch-to-buffer this-buff))
        (user-error "The buffer %S doesn't seem to be a part of a project" (buffer-name))))))

;;;###autoload
(defun otpp-change-tab-root-dir (dir &optional tab-number)
  "Change the `otpp-root-dir' attribute to DIR.
If if the absolute TAB-NUMBER is provided, set it, otherwise, set the
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
      (otpp-uniq-register tab-new-root-dir
                          :base (and otpp-project-name-function
                                     (funcall otpp-project-name-function tab-new-root-dir))
                          :map 'otpp--unique-tabs-map))
    (otpp--update-all-tabs) ; Update all tabs
    (run-hook-with-args 'otpp-post-change-tab-root-functions tab)))

(defun otpp-find-tabs-by-root-dir (dir)
  "Return a list of tabs that have DIR as `otpp-root-dir' attribute."
  (seq-filter
   (lambda (tab) (equal (expand-file-name dir) (otpp-get-tab-root-dir tab)))
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


;;; The `otpp-prefix' implementation

(defun otpp-prefix ()
  "Run the next command in the tab's root directory (or not!).

The actual behavior depends on `otpp-override-mode'. For
instance, when you execute \\[otpp-prefix] followed by
\\[project-find-file], if the `otpp-override-mode' is
enabled, this will run the `project-find-file' command in the
`default-directory', otherwise, it will bind the `default-directory' to
the current's tab directory before executing `project-find-file'."
  (interactive)
  (when-let* ((command (key-binding
                        (read-key-sequence
                         (format
                          "Run next command in %s "
                          (if otpp-override-mode "default-directory" "current's tab `ottp' root directory")))
                        t)))
    (let ((project-aware
           (or (string-match-p otpp-project-aware-commands-regexp (symbol-name command))
               (get command 'project-aware)))
          (root (if otpp-override-mode default-directory (otpp-get-tab-root-dir))))
      (when otpp-verbose (message "otpp: Running `%s' with `otpp-prefix'" command))
      (if project-aware
          (let ((project-current-directory-override root))
            (call-interactively command))
        (let ((default-directory root))
          (call-interactively command))))))


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
      (let ((curr-tab-root-dir (otpp-get-tab-root-dir))
            (target-proj-root-dir (expand-file-name proj-dir)))
        (unless (equal curr-tab-root-dir target-proj-root-dir)
          (if (or curr-tab-root-dir (otpp-find-tabs-by-root-dir target-proj-root-dir) otpp-preserve-non-otpp-tabs)
              (otpp-select-or-create-tab-root-dir target-proj-root-dir)
            (otpp-change-tab-root-dir target-proj-root-dir)))))
    proj-curr))

(defun otpp--project-switch-project-a (orig-fn &rest args)
  "Switch to the selected project's tab if it exists.
Call ORIG-FN with ARGS otherwise."
  (let ((proj-dir (expand-file-name (or (car args)
                                        (if (and (boundp 'project-prompter) (functionp project-prompter)) ; Emacs 30.1
                                            (funcall project-prompter)
                                          (project-prompt-project-dir))))))
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
        (setcdr (assq 'name curr-tab) (if (functionp otpp-default-tab-name) (funcall otpp-default-tab-name) otpp-default-tab-name))
        (setcdr (assq 'explicit-name curr-tab) 'def))
      (otpp-uniq-unregister curr-tab-root-dir :map 'otpp--unique-tabs-map)
      (otpp--update-all-tabs))))

(defun otpp--bury-on-kill-buffer-in-multiple-tabs-a (fn &optional buffer)
  "Advise `kill-buffer' FN to burry BUFFER when it is visible in other tabs."
  (if-let* ((tabs (and otpp-bury-on-kill-buffer-when-multiple-tabs
                       buffer (eq (get-buffer buffer) (current-buffer))
                       (tab-bar-get-buffer-tab buffer nil t t))))
      (progn
        (message "Buffer still alive in %d tab%s, burying it instead." (length tabs) (if (length> tabs 1) "s" ""))
        (bury-buffer))
    (funcall fn buffer)))


;;; OTPP modes

;;;###autoload
(define-minor-mode otpp-mode
  "Automatically create a tab per project, name them uniquely."
  :group 'otpp
  :global t
  (dolist (fn '(project-current project-switch-project project-kill-buffers))
    (let ((advice-fn (intern (format "otpp--%s-a" fn))))
      (if otpp-mode
          (advice-add fn :around advice-fn)
        (advice-remove fn advice-fn))))
  (if otpp-mode
      (progn
        (advice-add 'kill-buffer :around #'otpp--bury-on-kill-buffer-in-multiple-tabs-a))
    (advice-remove 'kill-buffer #'otpp--bury-on-kill-buffer-in-multiple-tabs-a)))

;;;###autoload
(define-minor-mode otpp-override-mode
  "Run commands in `otpp-override-commands' in the current's tab directory."
  :group 'otpp
  :global t
  (dolist (cmd otpp-override-commands)
    (if otpp-override-mode
        (advice-add cmd :around #'otpp--call-command-in-root-dir-maybe)
      (advice-remove cmd #'otpp--call-command-in-root-dir-maybe)))
  ;; Enable running the command in the current's tab directory
  (setq otpp-run-command-in-tab-root-dir otpp-override-mode))


(provide 'one-tab-per-project)
(provide 'otpp)
;;; otpp.el ends here
