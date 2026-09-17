Unlike [front-end scripts](#root/_help_yIhgI5H7A2Sm) which run on the client / browser-side, back-end scripts run directly on the Node.js environment of the Trilium server.

Back-end scripts can be used both on a <a class="reference-link" href="#root/_help_WOcw2SLH6tbX">Server Installation</a> (where it will run on the device the server is running on), or on the <a class="reference-link" href="#root/_help_poXkQfguuA0U">Desktop Installation</a> (where it will run on the PC).

> [!IMPORTANT]
> Starting with v0.104.0, backend scripts are disabled by default to reduce the attack surface. See <a class="reference-link" href="#root/_help_fiHicjpHjIRJ">Security</a> for more information.

## Advantages of backend scripts

The benefit of backend scripts is that they can be pretty powerful, for example to have access to the underlying system, for example it can read files or execute processes.

However, the main benefit of backend scripts is that they have easier access to the notes since the information about them is already loaded in memory. Whereas on the client, notes have to be manually loaded first.

## Creating a backend script

Create a new <a class="reference-link" href="#root/_help_6f9hih2hXXZk">Code</a> note and select the language _JavaScript (Trilium backend)_.

## Running backend scripts

Backend scripts can be either run manually (via the Execute button on the script page), or they can be triggered on certain events.

In addition, scripts can be run automatically when the server starts up, on a fixed time interval or when a certain event occurs (such as an attribute being modified). For more information, see the dedicated <a class="reference-link" href="#root/_help_GPERMystNGTB">Events</a> page.

## Script API

Trilium exposes a set of APIs that can be directly consumed by scripts, under the `api` object. For a reference of this API, see <a class="reference-link" href="#root/_help_MEtfsqa5VwNi">Backend API</a>.