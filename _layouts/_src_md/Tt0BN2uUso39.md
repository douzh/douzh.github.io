central entity in Trilium. Main attributes of note are [title](#root/_help_l7pigIGu01K2) and content.

### Note types

The main note type is a rich-text note type called [Text](#root/_help_iPIMuisry3hd). For diagrams and drawing there is  [Canvas](#root/_help_grjYqerjn243) and [Mermaid Diagrams](#root/_help_s1aBHPd79XYj).

There are also more complex note types such as [Saved Search](#root/_help_m523cpzocqaD), [Render Note](#root/_help_HcABDtFCkbFN) that usually go hand-in-hand with [Scripting](#root/_help_CdNpE2pqjmI6).

In Trilium there's no specific "folder" note type. Any note can have children and thus be a folder.

### Root note

There's one special note called "root note" which is root of the note tree. All other notes are placed below it in the structure.

### Tree structure

Importantly, note itself doesn't carry information on its placement in note tree. See [Cloning Notes](#root/_help_IakOLONlIfGI) for details.

Tree structure of notes can resemble file system - but compared to that notes in Trilium can act as both file and directory - meaning that note can both have its own content and have children. "Leaf note" is a note which doesn't have any children.

### Deleting / undeleting notes

When you delete a note in Trilium, it is actually only marked for deletion (soft-delete) - the actual content, title, attributes etc. are not deleted, only hidden.

Within (by default) 7 days, it is possible to undelete these soft-deleted notes - open the [Recent Changes](#root/_help_r5JGHN99bVKn) dialog, and you will see a list of all modified notes including the deleted ones. Notes available for undeletion have a link to do so. This is kind of "trash can" functionality known from e.g. Windows.

Clicking an undelete will recover the note, its content and attributes \- note should be just as before being deleted. This action will also undelete note's children which have been deleted in the same action.

To be able to undelete a note, it is necessary that deleted note's parent must be undeleted (otherwise there's no place where we can undelete it to). This might become a problem when you delete more notes in succession \- the solution is then undelete in the reverse order of your deletion.

After the 7 days (configurable) the notes will be "erased" - their title, content, revisions and attributes will be erased, and it will not be possible anymore to recover them (unless you restore a [Backup](#root/_help_ODY7qQn5m2FT)).

## See also

  * [Read-Only Notes](#root/_help_CoFPLs3dRlXc)

