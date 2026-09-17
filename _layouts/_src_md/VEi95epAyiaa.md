llows you to share selected notes as **publicly accessible** read-only documents. This feature is particularly useful for publishing content directly from your Trilium notes, making it accessible to others online.

![](Sharing_image.png)

## Features, interaction and limitations

  * Searching by note title.
  * Automatic dark/light mode based on the user's browser settings.
  * Mobile-friendly layout, with sidebar.
  * Collapsible tree with the same note icons as the application.
  * Customizable logo.
  * Toggle button for dark/light mode, which also stores the user preferences.
  * Quick navigation buttons (previous and next note).
  * Displaying the date of the last update of the note.

### By note type

  | Supported features | Limitations  
---|---|---  
[Text](#root/_help_iPIMuisry3hd) | 

  * Table of contents.
  * Syntax highlight of code blocks, provided a language is selected (does not work if “Auto-detected” is enabled).
  * Rendering for math equations.
  * [Including notes](#root/_help_nBAXQFj20hS1) (only if the included notes are also shared).

| 

  * Inline Mermaid diagrams are not rendered.

  
[Code](#root/_help_6f9hih2hXXZk) | 

  * Basic support (displaying the contents of the note in a monospace font).

| 

  * No syntax highlight.

  
[Saved Search](#root/_help_m523cpzocqaD) | Not supported. |    
[Relation Map](#root/_help_iRwzGnHPzonm) | Not supported. |    
[Note Map](#root/_help_bdUJEHsAPYQR) | Not supported. |    
[Render Note](#root/_help_HcABDtFCkbFN) | Not supported. |    
[Collections](#root/_help_GTwFsgaA0lCt) | 

  * The child notes are displayed in a fixed format. 

| 

  * More advanced view types such as the calendar view are not supported.

  
[Mermaid Diagrams](#root/_help_s1aBHPd79XYj) | 

  * The diagram is displayed as a vector image.

| 

  * No further interaction supported.

  
[Canvas](#root/_help_grjYqerjn243) | 

  * The diagram is displayed as a vector image.

| 

  * No further interaction supported.

  
[Web View](#root/_help_1vHRoWCEjj0L) | Not supported. |    
[Mind Map](#root/_help_gBbsAeiuUxI5) | The diagram is displayed as a vector image. | 

  * No further interaction supported.

  
[Geo Map](#root/_help_81SGnPGMk7Xc) | Not supported. |    
[File](#root/_help_W8vYD3Q1zjCR) | Basic interaction (downloading the file). | 

  * No further interaction supported.

  
  
While the sharing feature is powerful, it has some limitations:

  * **Code Notes** : No syntax highlighting.
  * **Static Note Tree**
  * **Protected Notes** : Cannot be shared.
  * **Include Notes** : Not supported.

Some of these limitations may be addressed in future updates.

## Prerequisites

To use the sharing feature, you must have a [Server Installation](#root/_help_WOcw2SLH6tbX) of Trilium. This is necessary because the notes will be hosted from the server.

## Sharing a note

  1. **Enable Sharing** : To share a note, toggle the `Shared` switch within the note's interface. Once sharing is enabled, an URL will appear, which you can click to access the shared note.

![Share Note](Sharing_share-single-note.png)

  2. **Access the Shared Note** : The link provided will open the note in your browser. If your server is not configured with a public IP, the URL will refer to `localhost (127.0.0.1)`.

## Sharing a note subtree

When you share a note, you actually share the entire subtree of notes beneath it. If the note has child notes, they will also be included in the shared content. For example, sharing the "Formatting" subtree will display a page with basic navigation for exploring all the notes within that subtree.

## Viewing and managing shared notes

You can view a list of all shared notes by clicking on "Show Shared Notes Subtree" in the [Global menu](#root/_help_x3i7MxGccDuM). This allows you to manage and navigate through all the notes you have made public.

## Security considerations

  * Shared notes are published on the open internet and can be accessed by anyone with the URL unless the notes are password-protected.
  * The URL's randomness does not provide security, so it is crucial not to share sensitive information through this feature.
  * Trilium takes precautions to protect your publicly shared instance from leaking information for non-shared notes, including opening a separate read-only connection to the [Database](#root/_help_wX4HbRucYSDD). Depending on your threat model, it might make more sense to use  [Exporting static HTML for web publishing](#root/_help_ycBFjKrrwE9p) and use battle-tested web servers such as Nginx or Apache to serve static content.

### Password protection

To protect shared notes with a username and password, you can use the `#shareCredentials`attribute. Add this label to the note with the format `#shareCredentials="username:password"`. To protect an entire subtree, make sure the label is [inheritable](#root/_help_bwZpz2ajCEwO).

## Advanced sharing options

### Customizing the appearance of shared notes

The default design should be a good starting point, but you can customize it using your own CSS:

  * **Custom CSS** : Link a CSS [Code](#root/_help_6f9hih2hXXZk) note to the shared page by adding a `~shareCss` relation to the note. If you want this style to apply to the entire subtree, make the label inheritable. You can hide the CSS code note from the tree navigation by adding the `#shareHiddenFromTree`label.
  * **Omitting Default CSS** : For extensive styling changes, use the `#shareOmitDefaultCss` label to avoid conflicts with Trilium's [default stylesheet](#root/_help_Wy267RK4M69c).

### Adding JavaScript

You can inject custom JavaScript into the shared note using the `~shareJs` relation. This allows you to access note attributes or traverse the note tree using the `fetchNote()` API, which retrieves note data based on its ID.

### Adding custom HTML

You can inject custom HTML snippets into specific locations of the shared page using the `~shareHtml` relation. The HTML note should contain the raw HTML content you want to inject, and you can control where it appears by adding the `#shareHtmlLocation` label to the HTML snippet note itself.

The `#shareHtmlLocation` label accepts values in the format `location:position`:

  * **Locations** : `head`, `body`, `content`
  * **Positions** : `start`, `end`

For example:

  * `#shareHtmlLocation=head:start` \- Injects HTML at the beginning of the `<head>` section
  * `#shareHtmlLocation=head:end` \- Injects HTML at the end of the `<head>` section (default)
  * `#shareHtmlLocation=body:start` \- Injects HTML at the beginning of the `<body>` section
  * `#shareHtmlLocation=content:start` \- Injects HTML at the beginning of the content area
  * `#shareHtmlLocation=content:end` \- Injects HTML at the end of the content area

If no location is specified, the HTML will be injected at `content:end` by default.

Example:
    
    
    const currentNote = await fetchNote();
    const parentNote = await fetchNote(currentNote.parentNoteIds[0]);
    
    for (const attr of parentNote.attributes) {
        console.log(attr.type, attr.name, attr.value);
    }

### Custom share templates

To completely redesign the share, it is possible to create or use an existing [custom share template](#root/_help_Ad7Ku5gOHrDU).

### Creating human-readable URL aliases

Shared notes typically have URLs like `http://domain.tld/share/knvU8aJy4dJ7`, where the last part is the note's ID. You can make these URLs more user-friendly by adding the `#shareAlias` label to individual notes (e.g., `#shareAlias=highlighting`). This will change the URL to `http://domain.tld/share/highlighting`.

**Important** :

  1. Ensure that aliases are unique.
  2. Using slashes (`/`) within aliases to create subpaths is not supported.

  * To easily identify pages that don't have a share alias, run a  [Search](#root/_help_eIg8jdvaoNNd) with `#!shareAlias`.
  * To be able to enter the share alias faster, consider using [Promoted Attributes](#root/_help_OFXdgB2nNk1F) (for example `#label:shareAlias(inheritable)="promoted,alias=Slug,single,text"`).

### Setting a custom favicon

To customize the favicon for your shared pages, create a relation `~shareFavicon`pointing to a file note containing the favicon (e.g., in `.ico` format).

### Sharing a note as the root

You can designate a specific note or folder as the root of your shared content by adding the `#shareRoot` label. This note will be linked when visiting `[http://domain.tld/share](http://domain/share)`, making it easier to use Trilium as a fully-fledged website.

Consider combining this with the `#shareIndex` label, which will display a list of all shared notes.

### Displaying an index of shared notes

When accessing a share, the sub-notes will be displayed in a tree on the left. But since multiple note trees can be shared, it might be useful to display a list of all the different share trees.

To do so, create a shared text note and apply the `shareIndex` label. When viewed, the list of shared roots will be displayed at the bottom of the note.

### Linking to an external website

Sometimes it's useful to include a link to an external website alongside your shared notes — for example in the shared navigation or in an index. To do so, add the `#shareExternalLink` label to a note, with the target URL as its value (e.g. `#shareExternalLink="https://example.com"`).

Any link pointing to this note will then redirect to the external website and open in a new browser tab, instead of opening the note's own shared page. This applies to:

  * the listing produced by the `#shareIndex` label;
  * the "Subpages" list shown under a parent note;
  * inline links to this note from within other shared notes.

The URL must be absolute and include the scheme (e.g. `https://`).

The note still exists in the share tree and its own page remains reachable by its direct URL — the label only changes how links to it behave.

## Attribute reference

Attribute | Description  
---|---  
`#shareHiddenFromTree` | this note is hidden from left navigation tree, but still accessible with its URL  
`#shareExternalLink` | when a link points to this note (from the share index, a subpage list, or an inline link in another shared note), it redirects to the given external URL in a new tab instead of the note's shared page. Value is the absolute URL, e.g. `https://example.com`.  
`#shareAlias` | define an alias using which the note will be available under `https://your_trilium_host/share/[your_alias]`  
`#shareOmitDefaultCss` | default share page CSS will be omitted. Use when you make extensive styling changes.  
`#shareRoot` | marks note which is served on /share root.  
`#shareDescription` | define text to be added to the HTML meta tag for description  
`#shareRaw` | Note will be served in its raw format, without HTML wrapper. See also  [Serving directly the content of a note](#root/_help_Qjt68inQ2bRj) for an alternative method without setting an attribute.  
`#shareDisallowRobotIndexing` |  Indicates to web crawlers that the page should not be indexed of this note by:

  * Setting the `X-Robots-Tag: noindex` HTTP header.
  * Setting the `noindex, follow` meta tag.

  
`#shareCredentials` | require credentials to access this shared note. Value is expected to be in format `username:password`. Don't forget to make this inheritable to apply to child-notes/images.  
`#shareIndex` | Note with this label will list all roots of shared notes.  
`#shareHtmlLocation` | defines where custom HTML injected via `~shareHtml` relation should be placed. Applied to the HTML snippet note itself. Format: `location:position`where location is `head`, `body`, or `content` and position is `start`or `end`. Defaults to `content:end`.  
  
### Customizing logo

It's possible to adjust the logo which is displayed on the top-left of the left pane.

Attribute | Description  
---|---  
`~shareLogo` | Relation set to an image to use as logo. The image must be part of the share tree (it can be hidden if needed).  
`#shareLogoWidth` | The width (in pixels, without unit) to set for the logo. Default is `53`.  
`#shareLogoHeight` | The height (in pixels, without unit) to set for the logo. Default is `40`.  
`#shareRootLink` | URL to navigate to when the logo is pressed.  
  
### Customizing OpenGraph

Attribute | Description  
---|---  
`#shareOpenGraphColor` | This adjusts the `theme-color` meta-property.  
`#shareOpenGraphURL` | This adjusts the `og:url` and `twitter:url` meta-properties.  
`#shareOpenGraphDomain` | Adjusts the `twitter:domain` meta-property.  
`#shareOpenGraphImage`    
`~shareOpenGraphImage` | Can be either a label, case in which the value is passed on as-is, or it can be a relation to an image [File](#root/_help_W8vYD3Q1zjCR). This controls the `og:image` meta-property.  
  
## Credits

Since v0.95.0, a new theme was introduced (and enabled by default) which greatly improves the visual aspect of the Share feature, as well as its functionality (such as mobile support, dark/light mode, collapsible tree, etc.). This theme is an adaptation of the [Trilium Rocks!](https://github.com/zerebos/trilium.rocks) by [zerebos](https://github.com/zerebos).
