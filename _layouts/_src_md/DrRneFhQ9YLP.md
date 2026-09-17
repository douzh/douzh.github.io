aspect of Trilium that allows it to have support for multiple  [Note Types](#root/_help_KSZ04uQ2D1St) is the fact that it makes use of various off-the-shelf or reusable libraries.

This page showcases some of the technologies used, for a better understanding of how Trilium works but also to credit the developers of that particular technology.

## CKEditor

CKEditor is the editor behind [Text](#root/_help_iPIMuisry3hd) notes, as well as integrated in various facets of the application such as the attribute editor in the [Status bar](#root/_help_AlJ73vBCjWDw) or the chat box in the [AI](#root/_help_GBBMSlVSOIGP) sidebar or notes.

For more information see [CKEditor](#root/_help_MI26XDLSAlCD).

## Excalidraw

[Excalidraw](https://excalidraw.com/) is the technology behind the [Canvas](#root/_help_grjYqerjn243) notes. The source code of the library is available on [GitHub](https://github.com/excalidraw/excalidraw).

We are using an unmodified version of it, so it shares the same [issues](https://github.com/excalidraw/excalidraw/issues) as the original.

## MapLibre GL JS

Trilium v0.105.0 introduces [MapLibre GL JS](https://github.com/maplibre/maplibre-gl-js) for the [Geo Map](#root/_help_81SGnPGMk7Xc) collection, which brings improved performance by using graphical acceleration (WebGL). Trilium ships its own track parsing logic as well as most of the UI such as the right panel.

## MindElixir

MindElixir is the library we are using for [Mind Map](#root/_help_gBbsAeiuUxI5) note types. The main library is available on [GitHub as mind-elixir-core](https://github.com/SSShooter/mind-elixir-core/issues).

Trilium ships its own UI which covers the toolbars, the contextual menu, the zoom buttons and the right panel that shows when clicking a node.

## FullCalendar

[FullCalendar](https://fullcalendar.io/) is the technology behind the [Calendar](#root/_help_xWbu3jpNWapp) collection, providing the various views (day, week, month, year) and the event management. Trilium also ships its own UI in the form of the popup that appears when editing, as well as the header that appears in the [Collection Properties](#root/_help_CssoWBu8I7jF).

Licensed under the MIT license.

## Editor c
