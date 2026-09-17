101.0, Trilium integrates Preact for front-end scripting, including support for JSX.

Preact can be used for:

  * [Render Note](#root/_help_HcABDtFCkbFN), where a JSX code note is used instead of a HTML one.
  * [Custom Widgets](#root/_help_MgibgPcfeuGz), where JSX can be used to replace the old, jQuery-based mechanism.

To get started, the first step is to enable JSX in the list of Code languages. Go to Options → Code Notes and check the “JSX” language. Afterwards, proceed with the documentation in either [Render Note](#root/_help_HcABDtFCkbFN) or  [Custom Widgets](#root/_help_MgibgPcfeuGz), which will both have a section on how to use the new Preact integration.

The documentation assumes prior knowledge with React or Preact. As a starting point, consider the [FreeCodeCamp course on Front End Development Libraries](https://www.freecodecamp.org/learn/front-end-development-libraries-v9/) or the [Preact Tutorial](https://preactjs.com/tutorial/).

## Import/exports

When using Preact with JSX, there is a special syntax which provides ES-like imports. This `import` syntax makes way for a more intuitive that doesn't make use of global objects and paves the way for better auto-completion support that might be introduced in the future. 

### API imports

Instead of:
    
    
    api.showMessage("Hello");

the JSX version looks like this:
    
    
    import { showMessage } from "trilium:api";
    showMessage("hello");

### Preact API imports (hooks, components)

There's a new [Script API](#root/_help_GLks18SNjxmC) dedicated to Preact, which provides shared components that are also used by Trilium internally as well as hooks, for example.
    
    
    import { useState } from "trilium:preact";
    const [ myState, setMyState ] = useState("Hi");

### Exporting

JSX notes can export a component for use in [Render Note](#root/_help_HcABDtFCkbFN) or for [Component libraries](#root/_help_Bqde6BvPo05g): 
    
    
    export default function() {
        return (
            <>
                <p>Hello world.</p>
            </>
        );
    }

### Import/export are not required

These imports are syntactic sugar meant to replace the usage for the `api`global object (see [Script API](#root/_help_GLks18SNjxmC)). 

The `import` and `export` syntax work only for JSX notes. Standard/jQuery code notes still need to use the `api`global and `module.exports`.

## Under the hood

Unlike JavaScript, JSX requires pre-processing to turn it into JavaScript (just like TypeScript). To do so, Trilium uses [Sucrase](https://github.com/alangpierce/sucrase), a JavaScript library which processes the JSX to pure JavaScript. The processing is done each time a script is run (for widgets this happens at every program startup). If you notice any performance degradation due to long compilation, consider [reporting the issue](#root/_help_wy8So3yZZlH9) to us.
