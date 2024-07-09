One tab per project (otpp)
---

This package is a lightweight workspace management by providing a thin layer
between builtin packages `project` and `tab-bar`. The whole idea is to create a
tab per opened project while ensuring unique names for the created tabs (when
multiple opened projects have the same name).

This package has been inspired by `project-tab-groups` which creates a "tab
group" per project.

## Installation
This package is not yet on MELPA, you need to installed from the GitHub
repository.

```emacs-lisp
;; `one-tab-per-project' depends on `unique-dir-name', which is not on MELPA
(use-package unique-dir-name
  :straight (:host github :repo "abougouffa/unique-dir-name"))

(use-package one-tab-per-project
  :straight (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :init
  (otpp-mode 1))
```

## Usage
The usage is quite straightforward, there is no extra commands to learn to be
able to use it. When `otpp-mode` global minor mode is enabled, you will have
this:

- When you switch to a project `project-switch-project` (bound by default to
  `C-x p p`), `otpp` will create a tab with the project name.
- When you kill a project with all its buffers with `project-kill-buffers`, the
  tab is closed.
- Lets say you've switched to the project under `/home/user/project1/backend/`,
  `otpp` will create a tab named `backend` for this particular project. Now, you
  opened a second project under `/home/user/project2/backend/`, `otpp` will
  detect that the name of the project `backend` is the same as the previously
  opened one, but it have a different path. In this case, `otpp` will create a
  tab named `backend[project2]` and renames the previously opened tab to
  `backend[project1]`. This conflict resolution is provided by the
  [`unique-dir-name`](https://github.com/abougouffa/unique-dir-name) library,
  which works like the built-in `uniquify` library used to keep distinct names
  for buffer names.
- For some cases, you might need to attach a manually created tab (by
  `tab-bar-new-tab`) to an opened project so you have two tabs dedicated to the
  same project (with different windows layouts for example). In this case, you
  can call the command `otpp-change-tab-root-dir` and select the path of the
  project to attach to.

## Similar packages
This section is not exhaustive, it includes only the packages that I used before.

### [`project-tab-groups`](https://github.com/fritzgrabo/project-tab-groups)
This package provides a mode that enhances the Emacs built-in `project` to
support keeping projects isolated in named tab groups. `otpp` is inspired by
this package, but instead of setting the tab groups, `otpp` introduces a new
attribute in the tab named `otpp-root-dir` where it stores the root directory of
the project bound to the tab. This allows keeping the tabs updated in case
another project with the same name (but a different path) is opened.

### [`tabspaces`](https://github.com/mclear-tools/tabspaces)
This package provide workspace management with `tab-bar` and with an integration
with `project`. Contrary to `otpp` and `project-tab-groups`, `tabspaces` don't
create tabs automatically, you need to call specific commands like
`tabspaces-open-or-create-project-and-workspace`.
