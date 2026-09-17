ew layout_ is a series of UI/UX changes that were introduced in v0.101.0 that heavily change both existing UI elements, as well as adding some new ones. The goal of this new layout is to modernize the application and to make it more intuitive but at the same time to reduce clutter.

## Newly introduced features

### Status bar

At the bottom of the window there is a new bar called the _Status bar_. This bar houses multiple items such as the Breadcrumb navigation and information and settings about the current note, such as the [content language](#root/_help_veGu4faJErEM) and  [Attributes](#root/_help_zEY4DaJG4YT5).

For more information, consult the [dedicated page](#root/_help_AlJ73vBCjWDw).

![](5_New%20Layout_image.png)

### Inline title

In previous versions of Trilium, the title bar was fixed at all times. In the new layout, there is both a fixed title bar and one that scrolls with the text. The newly introduced title is called the _Inline title_ and it displays the title in a larger font, while also displaying additional information such as the creation and the modification date.

Whenever the title is scrolled past, the fixed title is shown instead.

This only affects [Text](#root/_help_iPIMuisry3hd) and  [Code](#root/_help_6f9hih2hXXZk) notes. Note types that take the entirety of the screen such as [Canvas](#root/_help_grjYqerjn243) will always have only the fixed title bar.

Depending on the note type, the inline title will also present some more interactive options such as being able to switch the note type (see below).

![](New%20Layout_image.png) The _Inline title_ , which is displayed at the top of the note and can be scrolled past. ![](4_New%20Layout_image.png) The fixed title bar. The title only appears after scrolling past the _Inline title_.

### New note type switcher

When a new [Text](#root/_help_iPIMuisry3hd) or  [Code](#root/_help_6f9hih2hXXZk) note is created, a note type switcher will appear below the _Inline title_. Apart from changing the note type, it's also possible to apply a [template](#root/_help_KC1HB96bqqHX).

The switcher will disappear as soon as a text is entered.

![](6_New%20Layout_image.png)

### Note badges

Note badges appear near the fixed note title and indicate important information about the note such as whether it is read-only. Some of the badges are also interactive.

![](3_New%20Layout_image.png)

The following badges are available:

  * **Read-only badge** , which will be shown if the note is not editable due to either automatic read-only or manual read-only. Clicking on the badge will temporarily edit the note (similar to the Edit [floating button](#root/_help_XpOYSgsLkTJy)).
  * **Share badge** , which will indicate that the current note is shared. The badge will also indicate if the share is on the local network (for the desktop application without [Synchronization](#root/_help_cbkrhQjrkKrh) set up) or publicly accessible (for the server).
  * **Web clip badge** , which will indicate if the note was clipped using the [Web Clipper](#root/_help_MtPxeAWVAzMg). The badge acts as a link, so it can be clicked on to navigate to the page or right clicked for more options.
  * **Execute badge** , for [scripts](#root/_help_CdNpE2pqjmI6) or [saved SQL queries](#root/_help_YKWqdJhzi2VY)which have an execute button or a description.

Some of these badges replace the dedicated panels at the top of the note.

### Collapsible sections

![](1_New%20Layout_image.png)

The following sections have been made collapsible:

  * _Promoted Attributes_
    * For full-height notes such as [Canvas](#root/_help_grjYqerjn243), the promoted attributes are collapsed by default to make room.
    * The keyboard shortcut previously used to trigger the promoted attributes ribbon tab (which was no longer working) has been repurposed to toggle the promoted attributes instead.
  * _Edited Notes_ , which appears for [[missing note]](#root/_help_l0tKav7yLHGF) is now shown underneath the title. 
    * Whether the section is collapsed or not depends on the choice in  [Options](#root/_help_4TIF1oA4VQRO) → Appearance.
  * _Search Properties_ , which appears for the full [Search](#root/_help_eIg8jdvaoNNd) and [Saved Search](#root/_help_m523cpzocqaD).

### Save status indicator

![](2_New%20Layout_image.png)To the right of the note title, a temporary indicators appears after making a change to the document that indicates whether the document has been saved.

It indicates the following states:

  * _Unsaved_ , if the changes will be saved soon.
  * _Saving_ , if the changes are being saved.
  * _Saved_ , if all the changes were successfully saved to the server.
  * _Error_ , if the changes could not be saved, for example due to a communication server with the server.

After all changes have been saved, the indicator will hide automatically after a few seconds.

## Changing to the existing layout

### Removal of the ribbon

The most significant change is the removal of the ribbon. All the actions and options from the ribbon were integrated in other places in the application.

Here's how all the different tabs that were once part of the ribbon are now available in the new layout:

  * “Formatting toolbar” was relocated to the top of the page. 
    * Instead of having one per split, now there is a single formatting toolbar per tab. This allows more space for the toolbar items.
  * “Owned attributes” and “Inherited attributes” were merged and moved to the status bar region (displayed one above the other).
  * “Basic Properties” were integrated in the [Note buttons](#root/_help_8YBEPzcpUgxw) menu. 
    * The only exception here is the Language combo box which can now be found in the status bar (top-right of the screen).
  * “File” and “Image” tabs 
    * The buttons were moved to the right of the note title, as dedicated entries in [Note buttons](#root/_help_8YBEPzcpUgxw).
    * The info section has been merged into the _Note info_ section of the status bar.
  * Edited notes 
    * Moved underneath the title, displayed under a collapsible area and the notes are represented as badges/chips.
    * Whether the section is expanded or collapsed depends on the “Edited Notes ribbon tab will automatically open on day notes” setting from Options → Appearance.
  * Search definition tab 
    * Moved underneath the title under a collapsible area.
    * Expanded by default for new searches, collapsed for saved searches.
  * The Note map is now available in the Note actions menu. 
    * Instead of opening into a panel in the ribbon, the note map now opens in the sidebar, in the [Connections tab](#root/_help_KRlZFoIH0j6p) where it can also be maximized.
  * “Note info” tab was moved to a small (i) icon in the status bar.
  * “Similar notes” tab 
    * Instead of opening into a panel in the ribbon, the similar notes are now shown in the sidebar, in the [Connections tab](#root/_help_KRlZFoIH0j6p).
  * The Collection properties tab were relocated under the note title and grouped into: 
    * A combo box to quickly switch between views.
    * Individual settings for the current view in a submenu.
  * Some smaller ribbon tabs were converted to badges that appear near the note title in the breadcrumb section: 
    * Original URL indicator for clipped web pages (`#pageUrl`).
    * SQL and script execute buttons.

The ribbon keyboard shortcuts (e.g. `toggleRibbonTabClassicEditor`) have been repurposed to work on the new layout, where they will toggle the appropriate panel.

### Removal of the floating buttons

Most of the buttons were relocated to the right of the note title, in the [Note buttons](#root/_help_8YBEPzcpUgxw) area, with the exception of:

  * The Edit button is displayed near the note title, as a badge.
  * _Backlinks_ are now displayed in the sidebar, in the [Connections tab](#root/_help_KRlZFoIH0j6p). 
    * Alternatively, the backlinks count is displayed in the status bar and when clicked the sidebar opens at the right section.
  * Relation map zoom buttons are now part of the relation map itself.
  * Export image to PNG/SVG are now in the Note actions menu, in the _Export as image_ option.

### Changes to the sidebar

The sidebar (also known as the right pane) also received some important changes.

Most importantly, v0.105.0 splits the sidebar into multiple tabs with additional functionality:

  * [[missing note]](#root/_help_2dcGDhp9WFKb), which gathers the table of contents and highlights lists.
  * [[missing note]](#root/_help_uNtgN0cUKEBc), which provides a graphical method of editing labels, relations and (promoted) attribute definitions.
  * A dedicated [[missing note]](#root/_help_LrKznOtw9cQD).
  * [Connections tab](#root/_help_KRlZFoIH0j6p), which groups together the note map, note paths, backlinks and similar notes.

The previous iteration of the sidebar would appear contextually, depending on whether there are any items to be displayed. This caused occasional content shifts when moving between two panes in a split view. In the new layout, the sidebar acts more like the [Note Tree](#root/_help_oPVyFC7WL2Lp) pane, remaining visible even if there is nothing to display.

In order to toggle the sidebar, there is a new button on the top-right side of the screen, near the window buttons (on Windows and Linux).

Now each section of the sidebar (e.g. “Table of Contents”, “Highlights list”) is individually collapsible and will remember whether it was collapsed.

Some sidebar items also have a contextual menu, indicated by the three dots on the title. For example, the highlights filter can be adjusted directly from that menu. 

Custom widgets are still supported. For custom scripts, the three dots menu allows quick navigation to the corresponding script note.

## How to toggle the new layout

Starting with v0.101.0, this new layout is enabled by default. It is possible to fall back to the old layout by going to [Options](#root/_help_4TIF1oA4VQRO) → Appearance and selecting _Old layout_.

Since a new layout was introduced, this becomes the standard one. The _Old layout_ is considered deprecated and will not receive new features (for example, the breadcrumb) as we focus on the new one. At some point the old layout will be removed entirely, as maintaining two layouts with major differences creates a maintenance burden.
