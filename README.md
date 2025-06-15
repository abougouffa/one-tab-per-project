<a href="https://github.com/abougouffa/one-tab-per-project"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## otpp.el
*One tab per project, with unique names*

---
[![MELPA](http://melpa.org/packages/otpp-badge.svg)](http://melpa.org/#/otpp)
[![MELPA Stable](http://stable.melpa.org/packages/otpp-badge.svg)](http://stable.melpa.org/#/otpp)

This is a lightweight workspace management package that provides a thin layer
between builtin packages `project` and `tab-bar`. The whole idea consists of
creating a _tab per opened project_ while ensuring unique names for the
created tabs (when multiple opened projects have the same name).

This package is inspired by `project-tab-groups` which creates a "tab group"
per project.

### Installation


This package is available on MELPA.

```emacs-lisp
(use-package otpp
  :straight t
  :after project
  :init
  ;; Enable `otpp-mode` globally
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1))
```

### Basic usage


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
  `otpp-uniq-*` routines.

- For some cases, you might need to attach a manually created tab (by
  `tab-bar-new-tab`) to an opened project so you have two tabs dedicated to
  the same project (with different windows layouts for example). In this
  case, you can call the command `otpp-change-tab-root-dir` and select the
  path of the project to attach to.

- When you use some commands to jump to a file (`find-file`,
  `xref-find-definitions`, etc.), you can end up with a buffer belonging to a
  _different project (lets say `B`)_ but displayed in the current project's
  tab _(`A`)_. In this case, you can call `otpp-detach-buffer-to-tab` to
  create a new tab dedicated to the buffer's project `B`. When the opened
  buffer is project-less (not part of a project), the command will signal a
  user error unless `otpp-allow-detach-projectless-buffer` is non-nil, in
  this case, `otpp` creates a new project-less tab for the buffer.

### Advanced usage


Consider this use case: supposing you are using `otpp-mode` and you've run
`project-switch-project` to open the `X` project in a new `X` tab. Now you
`M-x find-file` then you open the `test.cpp` file outside the current `X`
project. Now, if you run `project-find-file`, you will be in one of these two
situations:

1. If `test.cpp` is part of another project `Y`, the `project-find-file` will
   prompt you with a list of `Y`s files even though we are in the `X` tab.

2. If `test.cpp` isn't part of any project, `project-find-file` will prompt
you to select a project first, then to select a file.

For this, `otpp` provides `otpp-prefix` (we recommend to bind it to some key,
like `C-x t P`, using `otpp-prefix` from `M-x` can have some limitations).
When you run `otpp-prefix` followed by `C-x p f` for example, you will be
prompted for files in the current's tab project files even if you are
visiting a file outside of the current project.

In my workflow, I would like to always restrict the commands like
`project-find-file` and `project-kill-buffers` to the project bound to the
current tab, even if I'm visiting a file which is not part of this project.
If you like this behavior, you can enable the `otpp-override-mode`. This mode
will advice all the commands defined in `otpp-override-commands` to be ran in
the current's tab root directory (_a.k.a._, in the project bound to the
current tab).

When `otpp-override-mode` is enabled, the `otpp-prefix` acts inversely. While
all `otpp-override-commands` are restricted to the current's tab project by
default, running a command with `otpp-prefix` will disable this behavior,
which results of the next command to be run in the `default-directory`
depending on the visited buffer.

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
  `tabspaces-open-or-create-project-and-workspace`. Also, `tabspaces`
  behavior isn't predictable when you open several projects with the same
  directory name.



### Customization Documentation

#### `otpp-bury-on-kill-buffer-when-multiple-tabs`

Bury the current buffer when killed but it is opened in another tab.

When non-nil, this modifies the behavior of `kill-buffer` when killing
the current buffer. If the current buffer is opened in another tab, we
bury it instead of killing it. This only affects the current buffer,
when we explicitly select another buffer to kill, `otpp` assumes that we
have a good reason to kill it.

#### `otpp-reconnect-tab`

Whether to reconnect a disconnected tab when switching to it.

When set to a function's symbol, that function will be called
with the switched-to project's root directory as its single
argument.

When non-nil, show the project dispatch menu instead.

#### `otpp-strictly-obey-dir-locals`

Whether to strictly obey local variables.

Set a nil (default value) to only respect the local variables when they
are defined in the project's root (the `dir-locals-file` is located in
the project's root).

Set to a function that takes DIR, PROJECT-ROOT and DIR-LOCALS-ROOT as
arguments in this order, see the function `otpp-project-name`. The
function should return non-nil to take the local variables into account.

This can be useful when the project include sub-projects (a Git
repository with sub-modules, a Git repository with other Git repos
inside, a Repo workspace, etc).

#### `otpp-post-change-tab-root-functions`

List of functions to call after changing the `otpp-root-dir` of a tab.
This hook is run at the end of the function `otpp-change-tab-root-dir`.
The current tab is supplied as an argument.

#### `otpp-project-name-function`

Derive project name from a directory.

This function receives a directory and return the project name
for the project that includes this path.

#### `otpp-allow-detach-projectless-buffer`

Allow detaching a buffer to a new tab even if it is not part of a project.
This can also be set to a function that receives the buffer, and return
non-nil if we should allow the tab creation.

#### `otpp-override-commands`

A list of commands to be advised in `otpp-override-mode`.
These commands will be run with `default-directory` set the to current's
tab directory.

#### `otpp-default-tab-name`

The default tab name to use when the last otpp tab is killed.

#### `otpp-rename-the-initial-tab`

Rename the initial tab to the default name.
When `otpp-mode` is enabled and only one tab exists, rename it to
`otpp-default-tab-name`.

#### `otpp-project-aware-commands-regexp`

A regular expression to detect project-aware commands in `otpp-prefix`.

### Function and Macro Documentation

#### `(otpp-get-tab-root-dir &optional TAB)`

Get the root directory set to the TAB, default to the current tab.

#### `(otpp-project-name DIR)`

Get the project name from DIR.
This function extracts the project root. Then, it tries to find a
`dir-locals-file` file that can be applied to files inside the directory
DIR. When found, the local variables are read if any of these conditions
is correct:
- `otpp-strictly-obey-dir-locals` is set to a function, and calling it
  returns non-nil (we pass to this function the DIR, the project root
  and the directory containing the `dir-locals-file`).
- `otpp-strictly-obey-dir-locals` is a *not* a function and it is
  non-nil.
- The `dir-locals-file` file is stored in the project root, a.k.a.,
  the project root is the same as the `dir-locals-file` directory.
Then, this function checks in this order:
1. If the local variable `otpp-project-name` is set locally in the
`dir-locals-file`, use it as project name.
2. Same with the local variable `project-vc-name`.
3. Return the directory name of the project's root.
When DIR isn't part of any project, returns nil.

#### `(otpp-find-tabs-by-root-dir DIR)`

Return a list of tabs that have DIR as `otpp-root-dir` attribute.

#### `(otpp-detach-buffer-to-tab BUFFER)`

Create or switch to the tab corresponding to the project of BUFFER.
When called with the a prefix, it asks for the buffer.

#### `(otpp-change-tab-root-dir DIR &optional TAB-NUMBER)`

Change the `otpp-root-dir` attribute to DIR.
If if the absolute TAB-NUMBER is provided, set it, otherwise, set the
current tab.
When DIR is empty or nil, delete it from the tab.

#### `(otpp-prefix)`

Run the next command in the tab's root directory (or not!).
The actual behavior depends on `otpp-override-mode`. For
instance, when you execute M-x otpp-prefix followed by
C-x p f, if the `otpp-override-mode` is
enabled, this will run the `project-find-file` command in the
`default-directory`, otherwise, it will bind the `default-directory` to
the current's tab directory before executing `project-find-file`.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
