upports creating [Code](#root/_help_6f9hih2hXXZk) notes, i.e. notes which allow you to store some programming code and highlight it. Special case is JavaScript code notes which can also be executed inside Trilium which can in conjunction with [Script API](#root/_help_GLks18SNjxmC) provide extra functionality.

## Architecture Overview

To go further I must explain basic architecture of Trilium - in its essence it is a classic web application - it has these two main components:

  * frontend running in the browser (using HTML, CSS, JavaScript) - this is mainly used to interact with the user, display notes etc.
  * backend running JavaScript code in node.js runtime - this is responsible for e.g. storing notes, encrypting them etc.

So we have frontend and backend, each with their own set of responsibilities, but their common feature is that they both run JavaScript code. Add to this the fact, that we're able to create JavaScript [Code](#root/_help_6f9hih2hXXZk) and we're onto something.

## Use cases

  * ["New Task" launcher button](#root/_help_TjLYAo3JMO8X)

## Action handler

Saving the note to the database is backend's responsibility, so we immediately pass control to the backend and ask it to create a note. Once this is done, we show the newly created note so that the user can set the task title and maybe some attributes.

## Script execution

So we have a script which will add the button to the toolbar. But how can we execute it? One possibility is to click on "play" icon (marked by red circle). The problem with this is that this UI change is time bound by Trilium runtime so when we restart Trilium, button won't be there.

We need to execute it every time Trilium starts up, but we probably don't want to have to manually click on play button on every start up.

The solution is marked by red circle at the bottom - this note has [label](#root/_help_zEY4DaJG4YT5) `#run=frontendStartup`\- this is one of the "system" labels which Trilium understands. As you might guess, this will cause all such labeled script notes to be executed once Trilium frontend starts up.

(`#run=frontendStartup` does not work for [Mobile frontend](#root/_help_RDslemsQ6gCp)\- if you want to have scripts running there, give the script `#run=mobileStartup` label).

### Execute button

Runnable code notes (frontend or backend) and saved SQL consoles can optionally have a dedicated execute button alongside a description.

To do so, apply the following [labels](#root/_help_HI6GBBIduIgv):

  * A `#executeButton` with the value of the label being displayed as the label of the button.
  * An optional `#executeDescription` which adds explanatory text beside it.

## Autocomplete & linting

Starting with Trilium v0.104.0, frontend scripts, backend scripts and render notes benefit from an autocomplete system.

The autocomplete triggers automatically when typing `.` or manually by pressing `Ctrl`+`Space`.

In addition to that, the editor will also display syntax errors and warnings such as unreachable code.

If you notice a false positive in regards with the errors/warnings reported or an incorrect or missing API, feel free to [open a issue](#root/_help_wy8So3yZZlH9) with a code sample.

## More showcases

You can see more scripting with explanation in [Advanced Showcases](#root/_help_5668rwcirq1t).

## Events

See [Events](#root/_help_GPERMystNGTB).

## Script API

See [Script API](#root/_help_GLks18SNjxmC).

Unlike 
