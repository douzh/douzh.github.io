scripts are custom JavaScript notes that are run on the client (browser environment)

There are four flavors of front-end scripts:

|   
---|---  
Regular scripts | These are run with the current app and note context. These can be run either manually or automatically on start-up.  
[Custom Widgets](#root/_help_MgibgPcfeuGz) | These can introduce new UI elements in various positions, such as near the [Note Tree](#root/_help_oPVyFC7WL2Lp), content area or even the [Right Sidebar](#root/_help_RnaPdbciOfeq).  
[Launch Bar Widgets](#root/_help_4Gn3psZKsfSm) | Similar to [Custom Widgets](#root/_help_MgibgPcfeuGz), but dedicated to the [Launch Bar](#root/_help_xYmIYSP6wE3F). These can simply introduce new buttons or graphical elements to the bar.  
[Render Note](#root/_help_HcABDtFCkbFN) | This allows rendering custom content inside a note, using either HTML or Preact JSX.  
  
For more advanced behaviors that do not require a user interface (e.g. batch modifying notes), see [Backend scripts](#root/_help_SPirpZypehBG).

## Scripts

Scripts don't have any special requirements. They can be run manually using the _Execute_ button on the code note or they can be run automatically, see [Events](#root/_help_irkwjdNtjcxR).

## Widgets

Widgets require a certain format in order for Trilium to be able to integrate them into the UI.

  * For legacy widgets, the script note must export a `BasicWidget` or a derived one (see [Note context aware widget](#root/_help_GhurYZjh8e1V) or  [Right pane widget](#root/_help_M8IppdwVHSjG)).
  * For Preact widgets, a built-in helper called `defineWidget` needs to be used.

For more information, see [Custom Widgets](#root/_help_MgibgPcfeuGz).

## Script API

The front-end API of Trilium is available to all scripts running in the front-end context as global variable `api`. For a reference of the API, see [Frontend API](#root/_help_Q2z6av6JZVWm).

### Tutorial

For more information on building widgets, take a look at [Widget Basics](#root/_help_SynTBQiBsdYJ).

Custom wi
