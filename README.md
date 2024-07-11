<a href="https://github.com/abougouffa/one-tab-per-project"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## one-tab-per-project.el
*One tab per project, with unique names*

---

This package is a lightweight workspace management by providing a thin layer
between builtin packages `project` and `tab-bar`. The whole idea is to create
a tab per opened project while ensuring unique names for the created tabs
(when multiple opened projects have the same name).

This package has been inspired by `project-tab-groups` which creates a "tab
group" per project.

### Installation


This package is not yet on MELPA, you need to installed from the GitHub
repository.

```emacs-lisp
;; `one-tab-per-project` depends on `unique-dir-name`, which is not on MELPA
(use-package unique-dir-name
  :straight (:host github :repo "abougouffa/unique-dir-name"))

(use-package one-tab-per-project
  :straight (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :init
  (otpp-mode 1))
```

### Usage


The usage is quite straightforward, there is no extra commands to learn to be
able to use it. When `otpp-mode` global minor mode is enabled, you will have
this:

- When you switch to a project `project-switch-project` (bound by default to
  `C-x p p`), `otpp` will create a tab with the project name.

- When you kill a project with all its buffers with `project-kill-buffers`, the
  tab is closed.

- Lets say you've switched to the project under
  `/home/user/project1/backend/`, `otpp` will create a tab named `backend`
  for this particular project. Now, you opened a second project under
  `/home/user/project2/backend/`, `otpp` will detect that the name of the
  project `backend` is the same as the previously opened one, but it have a
  different path. In this case, `otpp` will create a tab named
  `backend[project2]` and renames the previously opened tab to
  `backend[project1]`. This conflict resolution is provided by the
  [`unique-dir-name`](https://github.com/abougouffa/unique-dir-name) library,
  which works like the built-in `uniquify` library used to keep distinct
  names for buffer names.

- For some cases, you might need to attach a manually created tab (by
  `tab-bar-new-tab`) to an opened project so you have two tabs dedicated to
  the same project (with different windows layouts for example). In this
  case, you can call the command `otpp-change-tab-root-dir` and select the
  path of the project to attach to.

### Similar packages


This section is not exhaustive, it includes only the packages that I used
before.

- [`project-tab-groups`](https://github.com/fritzgrabo/project-tab-groups):
  This package provides a mode that enhances the Emacs built-in `project` to
  support keeping projects isolated in named tab groups. `otpp` is inspired
  by this package, but instead of setting the tab groups, `otpp` introduces a
  new attribute in the tab named `otpp-root-dir` where it stores the root
  directory of the project bound to the tab. This allows keeping the tabs
  updated in case another project with the same name (but a different path)
  is opened.

- [`tabspaces`](https://github.com/mclear-tools/tabspaces): This package
  provide workspace management with `tab-bar` and with an integration with
  `project`. Contrary to `otpp` and `project-tab-groups`, `tabspaces` don't
  create tabs automatically, you need to call specific commands like
  `tabspaces-open-or-create-project-and-workspace`.



### Customization Documentation

#### `otpp-preserve-non-otpp-tabs`

When non-nil, preserve the current rootless tab when switching projects.

#### `otpp-reconnect-tab`

Whether to reconnect a disconnected tab when switching to it.

When set to a function's symbol, that function will be called
with the switched-to project's root directory as its single
argument.

When non-nil, show the project dispatch menu instead.

#### `otpp-strictly-obey-dir-locals`

Whether to strictly obey local variables.

Set a nil (default value) for only respect the variables when the
are defined in the project root.

Set to a function that takes (DIR PROJECT-ROOT DIR-LOCALS-ROOT),
see `otpp--project-name`.

This can be useful when the project includes sub-projects (a Git
repository with sub-modules, a Git repository with other Git
repos inside, etc).

#### `otpp-post-change-tab-root-functions`

List of functions to call after changing the `otpp-root-dir` of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir`.
The current tab is supplied as an argument.

#### `otpp-project-name-function`

Derrive project name from a directory.

This function receives a directory and return the project name
for the project that includes this path.

### Function and Macro Documentation

#### `(otpp-change-tab-root-dir DIR &optional TAB-NUMBER)`

Change the `otpp-root-dir` attribute to DIR.
If if the obsolete TAB-NUMBER is provided, set it, otherwise, set the
current tab.
When DIR is empty or nil, delete it from the tab.

#### `(otpp-find-tabs-by-root-dir DIR)`

Return a list of tabs that have DIR as `otpp-root-dir` attribute.

#### `(otpp-select-or-create-tab-root-dir DIR)`

Select or create the tab with root directory DIR.
Returns non-nil if a new tab was created, and nil otherwise.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
