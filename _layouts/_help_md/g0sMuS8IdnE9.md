While Trilium is actively maintained and stable, encountering bugs is possible.

## General Quick Fix

The first step in troubleshooting is often a restart.

If you experience an UI issue, the frontend may have entered an inconsistent state. Reload the application by pressing <kbd>Ctrl</kbd> + <kbd>R</kbd>. This will reload the frontend.

If the issue persists or appears to be a backend problem, restart the entire application. For the desktop (Electron) build, simply close and reopen the window. If you're using a Docker build, restart the container.

## Broken Note Crashes Trilium

Certain problems, such as rendering a note with a faulty script, can cause Trilium to crash. If Trilium attempts to reload the problematic note upon restart, it will continue to crash.

To resolve this, use the `TRILIUM_START_NOTE_ID` environment variable to reset the open tabs to a single specified note ID (e.g., `root`). In Linux, you can set it as follows:

```
TRILIUM_START_NOTE_ID=root ./trilium
```

## Broken Script Prevents Application Startup

If a custom script causes Trilium to crash, and it is set as a startup script or in an active [custom widget](#root/_help_MgibgPcfeuGz), start Triliumin "safe mode" to prevent any custom scripts from executing:

```
TRILIUM_SAFE_MODE=true ./trilium
```

Depending on your Trilium distribution, you may have pre-made scripts available: `trilium-safe-mode.bat` and `trilium-safe-mode.sh`.

Once Trilium starts, locate and fix or delete the problematic note.

## Sync and Consistency Checks

Trilium periodically verifies the logical consistency of the database (e.g., ensuring every note has a parent). If inconsistencies are detected, you will be notified via the UI.

In such cases, file a bug report and attach an [anonymized database](#root/_help_x59R8J8KV5Bp) if necessary.

## Restoring Backup

Trilium makes regular automatic backups. If issues become severe, you can [restore from a backup](#root/_help_ODY7qQn5m2FT).

## Forgotten Password

See <a class="reference-link" href="#root/_help_lwAX5SvbrK2g">Resetting your password</a>.

## Reporting Bugs

Reporting bugs is highly valuable. Here are some tips:

*   Use GitHub issues for reporting: [https://github.com/TriliumNext/Trilium/issues](https://github.com/TriliumNext/Trilium/issues)
*   Refer to the [error logs](#root/_help_qzNzp9LYQyPT) page for information on providing necessary details.