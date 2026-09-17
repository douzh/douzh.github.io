For [script code notes](#root/_help_CdNpE2pqjmI6), Trilium offers an API that gives them access to various features of the application.

There are two APIs:

*   One for the front-end scripts: <a class="reference-link" href="#root/_help_Q2z6av6JZVWm">Frontend API</a>
*   One for the back-end scripts: <a class="reference-link" href="#root/_help_MEtfsqa5VwNi">Backend API</a>

In both cases, the API resides in a global variable, `api`, that can be used anywhere in the script.

For example, to display a message to the user the following front-end script can be used:

```
api.showMessage("Hello world.");
```

> [!NOTE]
> **Note**  
> The Script API is currently experimental and may undergo changes in future updates.