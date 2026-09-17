ium data is stored in a [SQLite](https://www.sqlite.org) database which contains all notes, tree structure, metadata, and most of the configuration. The database file is named `document.db` and is stored in the application's default [Data directory](#root/_help_tAassRL4RSQL).

## Demo Notes

When first starting Trilium, it will provide a set of notes to showcase various features of the application.

For more information see [Demo Notes](#root/_help_6tZeKvSHEUiB).

## Manually Modifying the Database

Trilium provides a lot of flexibility, and with it, opportunities for advanced users to tweak it. If you need to explore or modify the database directly, you can use a tool such as [SQLite Browser](https://sqlitebrowser.org/) to work directly on the database file.

See [Manually altering the database](#root/_help_oyIAJ9PvvwHX) for more information.

## How to Reset the Database

If you want to start again and:

  * Create and switch to a new blank knowledge base
  * Use a different knowledge base synced from a server or desktop application
  * Restore a backup

You can navigate to **Settings → Database** and click the **Start over** button. You'll be prompted to restart the application, with the option to back up your current database before proceeding.

For the web client, Trilium cannot restart automatically. You'll need to manually restart the server/container and then reload the web application.

After restarting, the setup screen will appear. You can choose to back up your existing database, then proceed with the setup menu as if it's a new installation. The existing database will only be overwritten after selecting to create a new database, sync with a server, or restore from a backup.

### The Legacy Method

If you are experimenting with Trilium and want to return it to its original state, you can do that by deleting the current database. When you restart the application, it will generate a new database containing the original demo notes.

To delete the database, simply go to the [data directory](#root/_help_tAassRL4RSQL) and delete the `document.db` file (and any other files starting with `document.db`).

If you do not need to preserve any configurations that might be stored in the `config.ini` file, you can just delete all of the [data directory's](#root/_help_tAassRL4RSQL) contents to fully restore the application to its original state. You can also review the [configuration](#root/_help_Gzjqa934BdH4) file to provide all `config.ini`values as environment variables instead.

When you 
