e outlines the steps to install Trilium on your own server. You might consider this option if you want to set up [synchronization](#root/_help_cbkrhQjrkKrh) or use Trilium in a browser - accessible from anywhere.

## Installation Options

There are several ways to install Trilium on a server, each with its own advantages:

  * **Recommended** : [Docker Installation](#root/_help_rWX5eY045zbE) \- Available for **AMD64** and **ARM** architectures.
  * [Packaged Server Installation](#root/_help_3tW6mORuTHnB)
  * [PikaPods managed hosting](https://www.pikapods.com/pods?run=trilium-next)
  * [Manual Installation](#root/_help_J1Bb6lVlwU5T)
  * [Kubernetes](#root/_help_DCmT6e7clMoP)
  * [Cloudron](https://www.cloudron.io/store/com.github.trilium.cloudronapp.html)
  * [HomelabOS](https://homelabos.com/docs/software/trilium/)
  * [NixOS Module](#root/_help_moVgBcoxE3EK)

The server installation includes both web and [mobile frontends](#root/_help_RDslemsQ6gCp).

## Configuration

After setting up your server installation, you may want to configure settings such as the port or enable [TLS](#root/_help_l2VkvOwUNfZj). Configuration is managed via the Trilium `config.ini` file, which is located in the [data directory](#root/_help_tAassRL4RSQL) by default. To begin customizing your setup, copy the provided `config-sample.ini` file with default values to `config.ini`.

You can also review the [configuration](#root/_help_Gzjqa934BdH4) file to provide all `config.ini` values as environment variables instead.

### Config Location

By default, `config.ini`, the [database](#root/_help_wX4HbRucYSDD), and other important Trilium data files are stored in the [data directory](#root/_help_tAassRL4RSQL). If you prefer a different location, you can change it by setting the `TRILIUM_DATA_DIR`environment variable:
    
    
    export TRILIUM_DATA_DIR=/home/myuser/data/my-trilium-data

### Disabling Authentication

See [Authentication](#root/_help_0hzsNCP31IAB).

## Reverse Proxy Setup

To configure a reverse proxy for Trilium, you can use either **nginx** or **Apache**. You can also check out the documentation stored in the Reverse proxy folder.

### nginx

Add the following configuration to your `nginx` setup to proxy requests to Trilium:
    
    
    location /trilium/ {
        proxy_pass http://127.0.0.1:8080/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }

To avoid limiting the size of payloads, include this in the `server {}` block:
    
    
    # Set to 0 for unlimited. Default is 1M.
    client_max_body_size 0;

### Apache

For an Apache setup, refer to the [Apache proxy setup](#root/_help_fDLvzOx29Pfg) guide.
