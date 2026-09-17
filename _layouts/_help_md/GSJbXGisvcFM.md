Launch bar widgets are a subset of <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a> that can be used to render custom buttons and widgets inside the <a class="reference-link" href="#root/_help_xYmIYSP6wE3F">Launch Bar</a>.

## Creating a launch bar widget for the desktop layout

Unlike <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a>, the process of setting up a launch bar widget is slightly different:

1.  Create a Code note of type _JavaScript (front-end)_ or JSX (for Preact-based widgets).
    *   The script itself uses the same concepts as <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a>, including the use of a `NoteContextAwareWidget`or a `BasicWidget` (according to needs).
    *   As examples in both legacy and Preact format, see <a class="reference-link" href="#root/_help_IPArqVfDQ4We">Note Title Widget</a> and <a class="reference-link" href="#root/_help_gcI7RPbaNSh3">Analog Watch</a>.
2.  Don't set `#widget`, as that attribute is reserved for <a class="reference-link" href="#root/_help_MgibgPcfeuGz">Custom Widgets</a>.
3.  In the <a class="reference-link" href="#root/_help_x3i7MxGccDuM">Global menu</a>, select _Configure launchbar_.
4.  In the _Visible Launchers_ section, select _Add a custom widget_.
5.  Give the newly created launcher a name (and optionally a name).
6.  In the <a class="reference-link" href="#root/_help_OFXdgB2nNk1F">Promoted Attributes</a> section, modify the _widget_ field to point to the newly created note.
7.  [Refresh](#root/_help_s8alTXmpFR61) the UI.

## Mobile layout

Custom launch bar widgets are also supported on the <a class="reference-link" href="#root/_help_RDslemsQ6gCp">Mobile Frontend</a>. To do so:

1.  Go to <a class="reference-link" href="#root/_help_x3i7MxGccDuM">Global menu</a> → _Advanced_ → _Show Hidden Subtree_.
2.  Look for _Mobile Launch Bar_ and follow the same creation mechanism as in the previous section.