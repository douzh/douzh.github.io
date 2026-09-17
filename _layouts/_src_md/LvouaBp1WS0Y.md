with version v0.104.0, Trilium's importer was expanded to application-specific imports. Unlike the standard HTML or [Markdown](#root/_help_Oau6X9rCuegd) import/export, these importers are tailored to support the features and note structure of a particular application.

## Supported applications

  * [Microsoft OneNote](#root/_help_GnhlmrATVqcH), by connecting to your account using Microsoft Graph.
  * [Notion](#root/_help_Y5mKeJ6dsCld), via a ZIP export.
  * [Google Keep](#root/_help_nhQVuO4zvIxi), via a ZIP export from _Google Takeout_ (Keep has no dedicated export mechanism).
  * [Evernote](#root/_help_syuSEKf2rUGr), via an ENEX export.
  * [Anytype](#root/_help_83zmPBJRgfnW), via a JSON export.
  * [Obsidian](#root/_help_iWD7wiIuMtgV), via a ZIP of the vault.

## Importing data from another application

To import from an application, there are two ways to access the import dialog:

  * In the [Note Tree](#root/_help_oPVyFC7WL2Lp), right click on a note and select _Import into note_.
  * In the [Note buttons](#root/_help_8YBEPzcpUgxw) area, select _Import files_.

A list of supported applications will appear at the top, each with their own configuration. Simply click one and proceed with the on-screen instructions.

## Acknowledgement

  * Trilium's importer is inspired by Obsidian's [Importer plugin](https://github.com/obsidianmd/obsidian-importer) (licensed under the MIT license), for example regarding the OneNote connection process or the Notion ID management.

Starting 
