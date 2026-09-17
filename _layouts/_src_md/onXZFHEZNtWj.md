dgets are a special subset of scripts that render graphical elements in certain parts of the application. These can be used to add new functionality to the Trilium application.

## Preact with JSX vs. vanilla jQuery

In older versions of Trilium, custom widgets were exclusively written in a combination of jQuery with Trilium's internal widget architecture (e.g., `BasicWidget`, `NoteContextAwareWidget`).

Starting with v0.101.0, custom widgets can also be written in JSX using the [Preact](#root/_help_KLsqhjaqh1QW) framework. Both legacy and Preact widgets have the same capabilities, with a single difference:

  * Preact widgets are content-sized by default whereas legacy widgets need `this.contentSized()`applied in the constructor. For more information, see the corresponding section in [Troubleshooting](#root/_help_gMkgcLJ6jBkg).

Wherever possible, widget examples will be both in the legacy and Preact format.

## Creating a custom widget

  1. Create a [Code](#root/_help_6f9hih2hXXZk) note.
  2. Set the language to: 
     1. JavaScript (frontend) for legacy widgets using jQuery.
     2. JSX for Preact widgets. You might need to go to Options → Code to enable the language first.
  3. Apply the `#widget` [label](#root/_help_HI6GBBIduIgv).

## Getting started with a simple example

Let's start by creating a widget that shows a message near the content area. Follow the previous section to create a code note, and use the following content.

### Legacy version (jQuery)
    
    
    class HelloCenterPane extends api.BasicWidget {
    
        constructor() {
            super();
            this.contentSized();
        }
    
        get parentWidget() { return "center-pane" }
    
        doRender() {
            this.$widget = $("<span>Center pane</span>");
        }
        
    }
    
    module.exports = new HelloCenterPane();

[Refresh the application](#root/_help_s8alTXmpFR61) and the widget should appear underneath the content area.

### Preact version
    
    
    import { defineWidget } from "trilium:preact";
    
    export default defineWidget({
        parent: "center-pane",
        render: () => <span>Center pane from Preact.</span>
    });

[Refresh the application](#root/_help_s8alTXmpFR61) and the widget should appear underneath the content area.

## Widget location (parent widget)

A widget can be placed in one of the following sections of the applications:

Value for `parentWidget` | Description | Sample widget | Special requirements  
---|---|---|---  
`left-pane` | Appears within the same pane that holds the [Note Tree](#root/_help_oPVyFC7WL2Lp). | Same as above, with only a different `parentWidget`. | None.  
`center-pane` | In the content area. If a split is open, the widget will span all of the splits. | See example above. | None.  
`note-detail-pane` |  In the content area, inside the note detail area. If a split is open, the widget will be contained inside the split. This is ideal if the widget is note-specific. | [Note context aware widget](#root/_help_GhurYZjh8e1V) | 

  * The widget must export a `class` and not an instance of the class (e.g. `no new`) because it needs to be multiplied for each note, so that splits work correctly.
  * Since the `class` is exported instead of an instance, the `parentWidget` getter must be `static`, otherwise the widget is ignored.

  
`right-pane` | In the [Right Sidebar](#root/_help_RnaPdbciOfeq), as a dedicated section. | [Right pane widget](#root/_help_M8IppdwVHSjG) | 

  * Although not mandatory, it's best to use a `RightPanelWidget` instead of a `BasicWidget` or a `NoteContextAwareWidget`.

  
  
To position the widget somewhere else, just change the value passed to `get parentWidget()`for legacy widgets or the `parent` field for Preact. Do note that some positions such as `note-detail-pane` and `right-pane`have special requirements that need to be accounted for (see the table above).

## Launch bar widgets

Launch bar widgets are similar to _Custom widgets_ but are specific to the [Launch Bar](#root/_help_xYmIYSP6wE3F). See [Launch Bar Widgets](#root/_help_4Gn3psZKsfSm) for more information.

## Custom position

The position of a custom widget is defined via a `position` integer.

In legacy widgets:
    
    
    class MyWidget extends api.BasicWidget {
    	// [..
    	get position() { return 10; }
    }

In Preact widgets:
    
    
    export default defineWidget({
        // [...]
        position: 10
    });

## Classic 
