ns are a unique type of note that don't have content, but instead display their child notes in various presentation methods.

## Main collections

|   
---|---  
![](Collections_collection_calendar.webp) | [Calendar](#root/_help_xWbu3jpNWapp)        
which displays a week, month or year calendar with the notes being shown as events. New events can be added easily by dragging across the calendar.  
![](Collections_collection_table.webp) | [Table](#root/_help_2FvYrpmOXm29)        
displays each note as a row in a table, with [Promoted Attributes](#root/_help_OFXdgB2nNk1F) being shown as well. This makes it easy to visualize attributes of notes, as well as making them easily editable.  
![](Collections_collection_board.webp) | [Kanban Board](#root/_help_CtBQqbwXDx1w)        
displays notes in columns, grouped by the value of a label. Items and columns can easily be created or dragged around to change their status.  
![](Collections_collection_geomap.webp) | [Geo Map](#root/_help_81SGnPGMk7Xc)        
which displays a geographical map in which the notes are represented as markers/pins on the map. New events can be easily added by pointing on the map.  
![](Collections_collection_presentation.webp) | [Presentation](#root/_help_zP3PMqaG71Ct)        
which shows each note as a slide and can be presented full-screen with smooth transitions or exported to PDF for sharing.  
  
## Classic collections

Classic collections are read-only mode and compiles the contents of all child notes into one continuous view. This makes it ideal for reading extensive information broken into smaller, manageable segments.

  * [Grid View](#root/_help_8QqnMzx393bx) which is the default presentation method for child notes (see [Note List](#root/_help_0ESUbbAxVnoK)), where the notes are displayed as tiles with their title and content being visible.
  * [List View](#root/_help_mULW0Q3VojwY) is similar to [Grid View](#root/_help_8QqnMzx393bx), but it displays the notes one under the other with the content being expandable/collapsible, but also works recursively.

Classic collections make use of pagination to support a large number of notes. The size of a page can be customized via `pageSize`.

## Creating a new collection

To create a new collections, right click in the [Note Tree](#root/_help_oPVyFC7WL2Lp) and look for the _Collections_ entry and select the desired type.

By default, collections come with a default configuration and sometimes even sample notes. To create a collection completely from scratch:

  1. Create a new note of type _Text_ (or any type).
  2. Change the [note type](#root/_help_KSZ04uQ2D1St) to _Collection_.
  3. In [Collection Properties](#root/_help_CssoWBu8I7jF), select the desired view type.
  4. Consult the help page of the corresponding view type in order to understand how to configure them.

## Configuration

To change the configuration of a collection or even switch to a different collection (e.g. from Kanban Board to a Calendar), see the [Collection Properties](#root/_help_CssoWBu8I7jF) bar at the top of the note.

## Archived notes

By default, [archived notes](#root/_help_MKmLg5x6xkor) will not be shown in collections. This behavior can be changed by going to  [Collection Properties](#root/_help_CssoWBu8I7jF) and checking _Show archived notes_.

Archived notes will be generally indicated by being greyed out as opposed to the normal ones.

## Hiding the child notes from the note tree

For collections with a large number of items, it can be helpful to hide the items from the note tree for performance reasons and to reduce clutter. This is especially useful for standalone collections, such as a geomap or a task board.

To do so, go to [Collection Properties](#root/_help_CssoWBu8I7jF) and select _Hide child notes in tree_.

## Advanced use cases

### Adding a description to a collection

To add a text before the collection, for example to describe it:

  1. Create a new collection.
  2. Change the [note type](#root/_help_KSZ04uQ2D1St) from _Collection_ to _Text_.

Now the text will be displayed above while still maintaining the collection view.

The only downside to this method is that [Collection Properties](#root/_help_CssoWBu8I7jF) will not be shown anymore. In this case, modify the attributes manually or switch back temporarily to the _Collection_ type for configuration purposes.

### Using saved search

Collections, by default, only display the child notes. However, it is possible to use the [Search](#root/_help_eIg8jdvaoNNd) functionality to display notes all across the tree, with advanced querying functionality.

To do so, simply start a [Search](#root/_help_eIg8jdvaoNNd) and go to the _Collection Properties_ tab in the [Ribbon](#root/_help_BlN9DFI679QC) and select a desired type of collection. To keep the search-based collection, use a [Saved Search](#root/_help_m523cpzocqaD).

While in search, none of the collections will not display the child notes of the search results. The reason is that the search might hit a note multiple times, causing an exponential rise in the number of results.

## Under the hood

Collections by themselves are simply notes with no content that rely on the [Note List](#root/_help_0ESUbbAxVnoK) mechanism (the one that lists the children notes at the bottom of a note) to display information.

By default, new collections use predefined [Templates](#root/_help_KC1HB96bqqHX) that are stored safely in the [Hidden Notes](#root/_help_2mUhVmZK8RF3) to define some basic configuration such as the type of view, but also some  [Promoted Attributes](#root/_help_OFXdgB2nNk1F) to make editing easier.

Collections don't store their configuration (e.g. the position on the map, the hidden columns in a table) in the content of the note itself, but as attachments.
