s="image"> ![](Attributes_image.png)

In Trilium, attributes are key-value pairs assigned to notes, providing additional metadata or functionality. There are two primary types of attributes:

  1. [Labels](#root/_help_HI6GBBIduIgv) can be used for a variety of purposes, such as storing metadata or configuring the behavior of notes. Labels are also searchable, enhancing note retrieval.

For more information, including predefined labels, see [Labels](#root/_help_HI6GBBIduIgv).

  2. [Relations](#root/_help_Cq5X6iKQop6R) define connections between notes, similar to links. These can be used for metadata and scripting purposes.

For more information, including a list of predefined relations, see  [Relations](#root/_help_Cq5X6iKQop6R).

These attributes play a crucial role in organizing, categorizing, and enhancing the functionality of notes.

## Types of attributes

Conceptually there are two types of attributes (applying to both labels and relations):

  1. **System attributes**   
As the name suggest, these attributes have a special meaning since they are interpreted by Trilium. For example the `color` attribute will change the color of the note as displayed in the [Note Tree](#root/_help_oPVyFC7WL2Lp) and links, and `iconClass` will change the icon of a note.
  2. **User-defined attributes**   
These are free-form labels or relations that can be used by the user. They can be used purely for categorization purposes (especially if combined with [Search](#root/_help_eIg8jdvaoNNd)), or they can be given meaning through the use of [Scripting](#root/_help_CdNpE2pqjmI6).

In practice, Trilium makes no direct distinction of whether an attribute is a system one or a user-defined one. A label or relation is considered a system attribute if it matches one of the built-in names (e.g. like the aforementioned `iconClass`). Keep this in mind when creating  [Promoted Attributes](#root/_help_OFXdgB2nNk1F) in order not to accidentally alter a system attribute (unless intended).

## Viewing the list of attributes

Both the labels and relations for the current note are displayed in the _Owned Attributes_ section of the [Ribbon](#root/_help_BlN9DFI679QC), where they can be viewed and edited. Inherited attributes are displayed in the _Inherited Attributes_ section of the ribbon, where they can only be viewed.

In the list of attributes, labels are prefixed with the `#` character whereas relations are prefixed with the `~` character.

## Attribute Definitions and Promoted Attributes

[Promoted Attributes](#root/_help_OFXdgB2nNk1F) create a form-like editing experience for attributes, which makes it easy to enhancing the organization and management of attributes

## Multiplicity

Attributes in Trilium can be "multi-valued", meaning multiple attributes with the same name can co-exist. This can be combined with [Promoted Attributes](#root/_help_OFXdgB2nNk1F) to easily add them.

## Attribute Inheritance

Trilium supports attribute inheritance, allowing child notes to inherit attributes from their parents. For more information, see [Attribute Inheritance](#root/_help_bwZpz2ajCEwO).
