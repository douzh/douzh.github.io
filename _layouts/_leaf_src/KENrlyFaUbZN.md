Prior to v0.104.0, the [desktop application](#root/_help_poXkQfguuA0U) would also open a network port so that it can offer access to <a class="reference-link" href="#root/_help_pgxEVkzLl1OP">ETAPI (REST API)</a> or even use it as a web server (see <a class="reference-link" href="#root/_help_nRqcgfTb97uV">Using the desktop application as a server</a>).

In order to reduce the attack surface, Trilium now enables these services only for the local device (e.g. `localhost`) instead of serving them over the LAN.

To better understand what is affected, refer to the following table:

| Feature | Network access OFF | Network access ON |
| --- | --- | --- |
| <a class="reference-link" href="#root/_help_MtPxeAWVAzMg">Web Clipper</a> | 🔒️ `localhost` only (still accessible to the browser extension on the same device) | 🌐 `localhost` + LAN |
| <a class="reference-link" href="#root/_help_pgxEVkzLl1OP">ETAPI (REST API)</a> | 🔒️ `localhost` only | 🌐 `localhost` + LAN |
| [LLM MCP](#root/_help_GBBMSlVSOIGP) (only if enabled in settings) | 🔒️ `localhost` only (needs auth) | 🌐 `localhost` + LAN (needs auth) |
| [Web app](#root/_help_nRqcgfTb97uV) | ❌️ completely disabled (403), only the desktop app is usable. | 🌐 `localhost` + LAN |
| <a class="reference-link" href="#root/_help_R9pX4DGra2Vt">Sharing</a> notes | ❌️ completely disabled (403), if you are using <a class="reference-link" href="#root/_help_cbkrhQjrkKrh">Synchronization</a> this will still work as part of the [server](#root/_help_WOcw2SLH6tbX). | 🌐 `localhost` + LAN |