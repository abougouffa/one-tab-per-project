

<a href="https://github.com/abougouffa/one-tab-per-project"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## otpp.el
*Una pestaña por proyecto, con nombres únicos*

---
[![MELPA](http://melpa.org/packages/otpp-badge.svg)](http://melpa.org/#/otpp)
[![MELPA Stable](http://stable.melpa.org/packages/otpp-badge.svg)](http://stable.melpa.org/#/otpp)

Este es un paquete de gestión de espacios de trabajo ligero que proporciona una capa delgada entre los paquetes integrados `project` y `tab-bar`. La idea principal consiste en crear una _pestaña por proyecto abierto_ mientras se garantiza que los nombres de las pestañas creadas sean únicos (cuando varios proyectos abiertos tienen el mismo nombre).

Este paquete se inspira en `project-tab-groups`, el cual crea un "grupo de pestañas" por proyecto.

### Instalación

Este paquete está disponible en MELPA.

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

### Uso básico

El uso es bastante sencillo, no hay comandos adicionales que aprender para poder utilizarlo. Cuando se habilita el modo menor global `otpp-mode`, obtendrás lo siguiente:

- Cuando cambias a un proyecto con `project-switch-project` (vinculado por defecto a `C-x p p`), `otpp` creará una pestaña con el nombre del proyecto.

- Cuando eliminas un proyecto con todos sus búferes usando `project-kill-buffers`, la pestaña se cierra.

- Supongamos que te has cambiado al proyecto en `/home/user/project1/backend/`, `otpp` creará una pestaña llamada `backend` para este proyecto en particular. Ahora, abres un segundo proyecto en `/home/user/project2/backend/`, `otpp` detectará que el nombre del proyecto `backend` es el mismo que el abierto anteriormente, pero tiene una ruta diferente. En este caso, `otpp` creará una pestaña llamada `backend[project2]` y renombrará la pestaña abierta anteriormente a `backend[project1]`. Esta resolución de conflictos es proporcionada por las rutinas `otpp-uniq-*`.

- En algunos casos, es posible que necesites adjuntar una pestaña creada manualmente (con `tab-bar-new-tab`) a un proyecto abierto para tener dos pestañas dedicadas al mismo proyecto (con diferentes disposiciones de ventanas, por ejemplo). En este caso, puedes ejecutar el comando `otpp-change-tab-root-dir` y seleccionar la ruta del proyecto al que deseas adjuntarla.

- Cuando usas algunos comandos para saltar a un archivo (`find-file`, `xref-find-definitions`, etc.), puedes terminar con un búfer que pertenece a un _proyecto diferente (digamos `B`)_ pero que se muestra en la pestaña del proyecto actual _(`A`)_ . En este caso, puedes ejecutar `otpp-detach-buffer-to-tab` para crear una nueva pestaña dedicada al proyecto `B` del búfer. Cuando el búfer abierto no pertenece a ningún proyecto (no es parte de un proyecto), el comando signalizará un error de usuario a menos que `otpp-allow-detach-projectless-buffer` sea distinto de nil; en este caso, `otpp` crea una nueva pestaña sin proyecto para el búfer.

### Uso avanzado

Considera este caso de uso: supongamos que estás usando `otpp-mode` y has ejecutado `project-switch-project` para abrir el proyecto `X` en una nueva pestaña `X`. Ahora ejecutas `M-x find-file` y abres el archivo `test.cpp` fuera del proyecto `X` actual. Ahora, si ejecutas `project-find-file`, te encontrarás en una de estas dos situaciones:

1. Si `test.cpp` es parte de otro proyecto `Y`, `project-find-file` te mostrará una lista de los archivos de `Y` aunque estemos en la pestaña `X`.

2. Si `test.cpp` no es parte de ningún proyecto, `project-find-file` te pedirá que selecciones un proyecto primero y luego que selecciones un archivo.

Para esto, `otpp` proporciona `otpp-prefix` (recomendamos vincularlo a alguna tecla, como `C-x t P`; usar `otpp-prefix` desde `M-x` puede tener algunas limitaciones). Cuando ejecutas `otpp-prefix` seguido de `C-x p f`, por ejemplo, se te pedirá que selecciones archivos del proyecto de la pestaña actual, incluso si estás visitando un archivo fuera del proyecto actual.

En mi flujo de trabajo, me gustaría restringir siempre comandos como `project-find-file` y `project-kill-buffers` al proyecto vinculado a la pestaña actual, incluso si estoy visitando un archivo que no forma parte de este proyecto. Si te gusta este comportamiento, puedes habilitar `otpp-override-mode`. Este modo aplicará *advice* a todos los comandos definidos en `otpp-override-commands` para que se ejecuten en el directorio raíz de la pestaña actual (_es decir_, en el proyecto vinculado a la pestaña actual).

Cuando `otpp-override-mode` está habilitado, `otpp-prefix` actúa de manera inversa. Mientras que todos los `otpp-override-commands` están restringidos al proyecto de la pestaña actual por defecto, ejecutar un comando con `otpp-prefix` desactivará este comportamiento, lo que resultará en que el siguiente comando se ejecute en el `default-directory` dependiendo del búfer visitado.

### Paquetes similares

Esta sección no es exhaustiva, incluye solo los paquetes que utilicé anteriormente.

- [`project-tab-groups`](https://github.com/fritzgrabo/project-tab-groups): Este paquete proporciona un modo que mejora el `project` integrado de Emacs para soportar el aislamiento de proyectos en grupos de pestañas con nombre. `otpp` se inspira en este paquete, pero en lugar de configurar grupos de pestañas, `otpp` introduce un nuevo atributo en la pestaña llamado `otpp-root-dir` donde almacena el directorio raíz del proyecto vinculado a la pestaña. Esto permite mantener las pestañas actualizadas en caso de que se abra otro proyecto con el mismo nombre (pero una ruta diferente).

- [`tabspaces`](https://github.com/mclear-tools/tabspaces): Este paquete proporciona gestión de espacios de trabajo con `tab-bar` e integración con `project`. A diferencia de `otpp` y `project-tab-groups`, `tabspaces` no crea pestañas automáticamente; necesitas ejecutar comandos específicos como `tabspaces-open-or-create-project-and-workspace`. Además, el comportamiento de `tabspaces` no es predecible cuando abres varios proyectos con el mismo nombre de directorio.

### Documentación de personalización

#### `otpp-bury-on-kill-buffer-when-multiple-tabs`

Enterrar el búfer actual al eliminarlo si está abierto en otra pestaña.

Cuando es distinto de nil, esto modifica el comportamiento de `kill-buffer` al eliminar el búfer actual. Si el búfer actual está abierto en otra pestaña, lo enterramos en lugar de eliminarlo. Esto solo afecta al búfer actual; cuando seleccionamos explícitamente otro búfer para eliminarlo, `otpp` asume que tenemos una buena razón para hacerlo.

#### `otpp-reconnect-tab`

Indica si se debe reconectar una pestaña desconectada al cambiar a ella.

Cuando se establece en el símbolo de una función, esa función se llamará con el directorio raíz del proyecto al que se cambió como su único argumento.

Cuando es distinto de nil, se muestra el menú de selección del proyecto en su lugar.

#### `otpp-strictly-obey-dir-locals`

Indica si se deben obedecer estrictamente las variables locales.

Establece nil (valor por defecto) para respetar las variables locales solo cuando se definen en la raíz del proyecto (el archivo `dir-locals-file` se encuentra en la raíz del proyecto).

Establece una función que tome DIR, PROJECT-ROOT y DIR-LOCALS-ROOT como argumentos en este orden, consulta la función `otpp-project-name`. La función debe devolver un valor distinto de nil para tener en cuenta las variables locales.

Esto puede ser útil cuando el proyecto incluye subproyectos (un repositorio Git con submódulos, un repositorio Git con otros repositorios Git dentro, un espacio de trabajo Repo, etc.).

#### `otpp-kill-project-buffers-on-tab-close`

Eliminar los búferes del proyecto al llamar a `tab-close`.

Puede ser nil, t, "ask" (como una cadena) o una función que devuelva uno de estos valores. Cuando se establece en "ask", otpp pedirá confirmación antes de eliminar los búferes del proyecto.

#### `otpp-post-change-tab-root-functions`

Lista de funciones a llamar después de cambiar `otpp-root-dir` de una pestaña.

Este gancho se ejecuta al final de la función `otpp-change-tab-root-dir`. Se proporciona la pestaña actual como argumento.

#### `otpp-tab-group-name-hook`

Un gancho que devuelve el nombre del grupo de pestañas para el agrupamiento automático.

La primera función que devuelva un valor distinto de nil determinará el nombre del grupo de pestañas.

#### `otpp-project-name-function`

Derivar el nombre del proyecto a partir de un directorio.

Esta función recibe un directorio y devuelve el nombre del proyecto para el proyecto que incluye esta ruta.

#### `otpp-project-name-local-variables`

Lista de variables locales a considerar para el nombre del proyecto.

Esto se utiliza con la función `otpp-project-name`.

#### `otpp-allow-detach-projectless-buffer`

Permitir desconectar un búfer a una nueva pestaña incluso si no pertenece a ningún proyecto.

También puede establecerse en una función que reciba el búfer y devuelva un valor distinto de nil si debemos permitir la creación de la pestaña.

#### `otpp-find-file-integration`

Cuando es distinto de nil, si se abre un archivo, cambia a su proyecto y pestaña. Crea la pestaña si el proyecto ya no está abierto.

#### `otpp-override-commands`

Una lista de comandos que serán aconsejados en `otpp-override-mode`.

Estos comandos se ejecutarán con `default-directory` establecido en el directorio de la pestaña actual.

#### `otpp-default-tab-name`

El nombre de pestaña por defecto a usar cuando se elimina la última pestaña de otpp.

#### `otpp-rename-the-initial-tab`

Renombrar la pestaña inicial al nombre por defecto.

Cuando `otpp-mode` está habilitado y solo existe una pestaña, renómbrala a `otpp-default-tab-name`.

#### `otpp-project-aware-commands-regexp`

Una expresión regular para detectar comandos conscientes del proyecto en `otpp-prefix`.

### Documentación de funciones y macros

#### `(otpp-with-internal-calls &rest BODY)` (macro)

Llamar a BODY con `otpp-internal-call` establecido en t.

#### `(otpp-get-tab-root-dir &optional TAB)`

Obtener el directorio raíz establecido para la TAB, por defecto usa la pestaña actual.

#### `(otpp-project-current &optional TAB)`

Devolver el proyecto de TAB (o de la pestaña actual).

#### `(otpp-project-name DIR)`

Obtener el nombre del proyecto a partir de DIR.
Esta función extrae la raíz del proyecto. Luego, intenta encontrar un archivo `dir-locals-file` que pueda aplicarse a los archivos dentro del directorio DIR. Cuando se encuentra, se leen las variables locales si se cumple alguna de estas condiciones:
- `otpp-strictly-obey-dir-locals` está establecido en una función y llamarla devuelve un valor distinto de nil (le pasamos a esta función DIR, la raíz del proyecto y el directorio que contiene `dir-locals-file`).
- `otpp-strictly-obey-dir-locals` *no* es una función y es distinto de nil.
- El archivo `dir-locals-file` está almacenado en la raíz del proyecto, es decir, la raíz del proyecto es la misma que el directorio de `dir-locals-file`.
Luego, esta función verifica si alguna de las variables locales en `otpp-project-name-local-variables` está establecida localmente en el `dir-locals-file`; cuando es así, usamos su valor como nombre del proyecto. De lo contrario, devolvemos el nombre del directorio de la raíz del proyecto.
Cuando DIR no es parte de ningún proyecto, devuelve nil.

#### `(otpp-find-tabs-by-root-dir DIR)`

Devolver una lista de pestañas que tienen DIR como atributo `otpp-root-dir`.

#### `(otpp-detach-buffer-to-tab BUFFER)`

Crear o cambiar a la pestaña correspondiente al proyecto de BUFFER.
Cuando se llama con un prefijo, pide el búfer.

#### `(otpp-change-tab-root-dir DIR &optional TAB-NUMBER)`

Cambiar el atributo `otpp-root-dir` a DIR.
Si se proporciona un TAB-NUMBER absoluto, se establece, de lo contrario, se establece la pestaña actual.
Cuando DIR está vacío o es nil, se elimina de la pestaña.

#### `(otpp-prefix)`

Ejecutar el siguiente comando en el directorio raíz de la pestaña (¡o no!).
El comportamiento real depende de `otpp-override-mode`. Por ejemplo, cuando ejecutas M-x otpp-prefix seguido de C-x p f, si `otpp-override-mode` está habilitado, esto ejecutará el comando `project-find-file` en el `default-directory`; de lo contrario, vinculará `default-directory` al directorio de la pestaña actual antes de ejecutar `project-find-file`.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Archivo README en Markdown generado por
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
