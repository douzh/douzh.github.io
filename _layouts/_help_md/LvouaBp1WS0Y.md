Starting with version v0.104.0, Trilium's importer was expanded to application-specific imports. Unlike the standard HTML or <a class="reference-link" href="#root/_help_Oau6X9rCuegd">Markdown</a> import/export, these importers are tailored to support the features and note structure of a particular application.

## Supported applications

*   <a class="reference-link" href="#root/_help_GnhlmrATVqcH">Microsoft OneNote</a>, by connecting to your account using Microsoft Graph.
*   <a class="reference-link" href="#root/_help_Y5mKeJ6dsCld">Notion</a>, via a ZIP export.
*   <a class="reference-link" href="#root/_help_nhQVuO4zvIxi">Google Keep</a>, via a ZIP export from _Google Takeout_ (Keep has no dedicated export mechanism).
*   <a class="reference-link" href="#root/_help_syuSEKf2rUGr">Evernote</a>, via an ENEX export.
*   <a class="reference-link" href="#root/_help_83zmPBJRgfnW">Anytype</a>, via a JSON export.
*   <a class="reference-link" href="#root/_help_iWD7wiIuMtgV">Obsidian</a>, via a ZIP of the vault.

## Importing data from another application

To import from an application, there are two ways to access the import dialog:

*   In the <a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">Note Tree</a>, right click on a note and select _Import into note_.
*   In the <a class="reference-link" href="#root/_help_8YBEPzcpUgxw">Note buttons</a> area, select _Import files_.

A list of supported applications will appear at the top, each with their own configuration. Simply click one and proceed with the on-screen instructions.

## Acknowledgement

*   Trilium's importer is inspired by Obsidian's [Importer plugin](https://github.com/obsidianmd/obsidian-importer) (licensed under the MIT license), for example regarding the OneNote connection process or the Notion ID management.