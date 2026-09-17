r widgets are a subset of [Custom Widgets](#root/_help_MgibgPcfeuGz) that can be used to render custom buttons and widgets inside the [Launch Bar](#root/_help_xYmIYSP6wE3F).

## Creating a launch bar widget for the desktop layout

Unlike [Custom Widgets](#root/_help_MgibgPcfeuGz), the process of setting up a launch bar widget is slightly different:

  1. Create a Code note of type _JavaScript (front-end)_ or JSX (for Preact-based widgets). 
     * The script itself uses the same concepts as [Custom Widgets](#root/_help_MgibgPcfeuGz), including the use of a `NoteContextAwareWidget`or a `BasicWidget` (according to needs).
     * As examples in both legacy and Preact format, see [Note Title Widget](#root/_help_IPArqVfDQ4We) and [Analog Watch](#root/_help_gcI7RPbaNSh3).
  2. Don't set `#widget`, as that attribute is reserved for [Custom Widgets](#root/_help_MgibgPcfeuGz).
  3. In the [Global menu](#root/_help_x3i7MxGccDuM), select _Configure launchbar_.
  4. In the _Visible Launchers_ section, select _Add a custom widget_.
  5. Give the newly created launcher a name (and optionally a name).
  6. In the [Promoted Attributes](#root/_help_OFXdgB2nNk1F) section, modify the _widget_ field to point to the newly created note.
  7. [Refresh](#root/_help_s8alTXmpFR61) the UI.

## Mobile layout

Custom launch bar widgets are also supported on the [Mobile Frontend](#root/_help_RDslemsQ6gCp). To do so:

  1. Go to [Global menu](#root/_help_x3i7MxGccDuM) → _Advanced_ → _Show Hidden Subtree_.
  2. Look for _Mobile Launch Bar_ and follow the same creation mechanism as in the previous section.

