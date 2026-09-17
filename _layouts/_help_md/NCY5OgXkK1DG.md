Front-end scripts are custom JavaScript notes that are run on the client (browser environment)

There are four flavors of front-end scripts:

|  |  |
| --- | --- |
| Regular scripts | These are run with the current app and note context. These can be run either manually or automatically on start-up. |
| <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a> | These can introduce new UI elements in various positions, such as near the <a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">Note Tree</a>, content area or even the <a class="reference-link" href="#root/_help_RnaPdbciOfeq">Right Sidebar</a>. |
| <a class="reference-link" href="#root/_help_4Gn3psZKsfSm">Launch Bar Widgets</a> | Similar to <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a>, but dedicated to the <a class="reference-link" href="#root/_help_xYmIYSP6wE3F">Launch Bar</a>. These can simply introduce new buttons or graphical elements to the bar. |
| <a class="reference-link" href="#root/_help_HcABDtFCkbFN">Render Note</a> | This allows rendering custom content inside a note, using either HTML or Preact JSX. |

For more advanced behaviors that do not require a user interface (e.g. batch modifying notes), see <a class="reference-link" href="#root/_help_SPirpZypehBG">Backend scripts</a>.

## Scripts

Scripts don't have any special requirements. They can be run manually using the _Execute_ button on the code note or they can be run automatically, see <a class="reference-link" href="#root/_help_irkwjdNtjcxR">Events</a>.

## Widgets

Widgets require a certain format in order for Trilium to be able to integrate them into the UI.

*   For legacy widgets, the script note must export a `BasicWidget` or a derived one (see <a class="reference-link" href="#root/_help_GhurYZjh8e1V">Note context aware widget</a> or  <a class="reference-link" href="#root/_help_M8IppdwVHSjG">Right pane widget</a>).
*   For Preact widgets, a built-in helper called `defineWidget` needs to be used.

For more information, see <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a>.

## Script API

The front-end API of Trilium is available to all scripts running in the front-end context as global variable `api`. For a reference of the API, see <a class="reference-link" href="#root/_help_Q2z6av6JZVWm">Frontend API</a>.

### Tutorial

For more information on building widgets, take a look at [Widget Basics](#root/_help_SynTBQiBsdYJ).