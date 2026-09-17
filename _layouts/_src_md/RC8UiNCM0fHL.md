ile_ note type can be used to attach various external files such as images, videos or PDF documents.

## Uploading a file

Since these files come from an external source, it is not possible to create a _File_ note type directly:

  * Drag a file into the [Note Tree](#root/_help_oPVyFC7WL2Lp).
  * Right click a note and select _Import into note_ and point it to one of the supported files.

## Supported file types

### PDFs

PDFs can be uploaded to Trilium and they will display a live preview with full support for various features such as table of contents, annotations, as well as remembering the last page read. For more information, see the dedicated [PDFs](#root/_help_XJGJrpu7F9sh) page.

## Office documents

Trilium offers previews for Office documents such as Word (`.docx`), Excel (`.xlsx`) and PowerPoint (`.pptx`). For more information, see the dedicated [Office documents](#root/_help_KG48zgNY31NW).

### Images

![](2_File_image.png)

Interaction:

  * _Copy reference to clipboard_ , for embedding the image within  [Text](#root/_help_iPIMuisry3hd) notes. 
    * See [Image references](#root/_help_0Ofbk1aSuVRu) for more information.
    * Alternatively, press the corresponding button from the [Floating buttons](#root/_help_XpOYSgsLkTJy).

### Videos

See [Audio & Video](#root/_help_AjqEeiDUOzj4).

### Audio

![](1_File_image.png)

Adding a supported audio file will reveal a basic audio player that can be used to play it.

Interactions:

  * The audio can be played/paused using the dedicated button.
  * Dragging the mouse across, or clicking the progress bar will seek through the song.
  * The volume can be set.
  * The playback speed can be adjusted via the contextual menu next to the volume.

### Text files

![](File_image.png)

Files that are identified as containing text will show a preview of their content. One common use case for this type of file is to embed text files whose content is not necessarily of interest to the user, such as third-party libraries or generated content, that can then be downloaded if needed.

Note that generally text files will be [imported](#root/_help_mHbBMPDPkVV5) as either [Text](#root/_help_iPIMuisry3hd) or  [Code](#root/_help_6f9hih2hXXZk) notes. To bypass this behavior and create a _File_ note type, use the _Import into note_ feature and uncheck _Import HTML, Markdown and TXT as text notes_ , as well as _Import recognized code files as code notes_. 

Since one of the use cases for having files instead of notes is to display large files, the content preview is limited to a relatively small amount of characters. To view the full file, consider opening it in an external application.

### GPS tracks

Trilium displays information about GPS tracks in `.gpx` format, such as the distance, duration, elevation profile, tracks, markers.

When a `.gpx` note is dropped in a  [Geo Map](#root/_help_81SGnPGMk7Xc), the track itself will also be displayed on the map.

![](4_File_image.png)

### Unknown file types

![](3_File_image.png)

If the file could not be identified as any of the supported file types from above, it will be treated as an unknown file. In this case, all the default interactions will be available such as downloading or opening the file externally, but there will be no preview of the content.

## Interaction

  * Regardless of the file type, a series of buttons will be displayed in the _Image_ or _File_ tab in the [Ribbon](#root/_help_BlN9DFI679QC). 
    * _Download_ , which will download the file for local use.
    * _Open_ , will will open the file with the system-default application.
    * Upload new revision to replace the file with a new one.
  * It is **not** possible to change the note type of a _File_ note.
  * Convert into an [attachment](#root/_help_0vhv7lsOLy82) from the [note menu](#root/_help_8YBEPzcpUgxw).

## Relation with other notes

  * Files are also displayed in the [Note List](#root/_help_0ESUbbAxVnoK) based on their type:

![](5_File_image.png)
  * Non-image files can be embedded into text notes as read-only widgets via the [Include Note](#root/_help_nBAXQFj20hS1) functionality.

  * Image files can be embedded into text notes like normal images via  [Image references](#root/_help_0Ofbk1aSuVRu).

## File size limit

An individual file cannot be bigger than 374 MiB. For more information see _Maximum import size_ in [Import & Export](#root/_help_mHbBMPDPkVV5).
