an connect to a large language model and use it as an assistant that works directly on your notes: ask questions about the note you're reading, have it draft or restructure content, or get it to write scripts and widgets for you.

The integration is off by default and does nothing until you enable it and configure a provider; Trilium ships no model of its own. Which provider you pick also decides where your notes travel: a cloud API billed per use, a subscription you already pay for, or a model running on your own hardware, in which case nothing leaves the machine. See [Providers](#root/_help_bkPZAMT7d2sR) for what each involves, and  [Privacy](#root/_help_cRRrdZjgyhNc) for exactly what gets sent.

Once enabled, the assistant is available as:

  * As a dedicated note type, it can read and modify notes through tools, which you can switch off per conversation if you would rather it only saw what you type.
  * A panel in the right sidebar, which acts similarly to the dedicated note type but it also has optional access to the current note.
  * An [In-editor AI assistant](#root/_help_VRoP0g1uHHc2) which comes with builtin actions that operate over selected text (such as proof-reading, summarizing), as well as [Custom AI quick actions](#root/_help_mVEYwt20exhO).

## Feature highlights

  * Chat-based interface with live streaming of the messages.
  * Provides the AI context with the note currently being looked at.
  * Tools to alter note content, create new notes, etc.
  * Statistics regarding context window use and pricing per message.
  * Attachments for multi-modal chat (images, text files, PDFs).
  * Optional MCP to allow external chat tools (e.g. Claude Code) to operate on notes within Trilium.

## Sample use-cases

  * Create any type of [Custom Widgets](#root/_help_MgibgPcfeuGz).
  * Easily create a [Render Note](#root/_help_HcABDtFCkbFN), e.g. _Create for me a render note which allows me to play tic-tac-toe. Make sure to use Preact instead of the legacy jQuery._
  * Create widgets for a [Dashboard](#root/_help_IF7Q6I9x7zuw), such as a calculator, a stopwatch, a pomodoro timer.

Claude Sonnet is known to produce very good frontend or backend scripts with little guidance, as the AI has been instructed in how to produce them.

## LLM Providers

Trilium supports four different types of providers:

  * **Cloud providers**   
Pay-per use with an API key which is billed separately from any subscription you might already have 
    * Anthropic (Claude)
    * OpenAI (GPT)
    * Google (Gemini)
    * DeepSeek
  * **Subscription-based**   
Reuses an existing subscription instead of paying per use. 
    * Currently only Claude Code is supported.
  * **Local or self-hosted LLM solutions**
    * Ollama
    * LM Studio.
  * **Custom OpenAI compatible endpoints**   
For other providers that are not directly supported by Trilium, either local or hosted (e.g. OpenRouter, Groq, Mistral).

For more information about each provider, see [Providers](#root/_help_bkPZAMT7d2sR). See the dedicated [Privacy](#root/_help_cRRrdZjgyhNc) to better understand what data is sent to providers.

## Enabling the AI integration

To enable the AI integration, simply go to [Options](#root/_help_4TIF1oA4VQRO) → _AI / LLM_ and press the toggle in the top-right of the dialog and configure a provider.

## Creating a new chat

There are two different chat interfaces:

  * One in the sidebar.
  * A dedicated note type.

### The sidebar interface

### The dedicated note type

The dedicated chat note is similar to the sidebar interface, but it makes longer conversations more comfortable to read.

Unlike the sidebar, the AI will not be aware of the current note it's in.

### Templates

Chat notes can be set as [Templates](#root/_help_KC1HB96bqqHX) to make them easily reusable. The entire conversation history is kept, allowing a basic form of specialization for the LLM with the existing chat acting like a system prompt. 

### Model selection

When a provider is configured in [Options](#root/_help_4TIF1oA4VQRO), the next step is to select the models that will be available for the chat.

The models are retrieved dynamically from the provider, only when the model selection list is visible. To alter the list of models, simply press the Edit button in the model selection box.

Pricing information is displayed for known models. The pricing information (price per million tokens) is embedded in the application (using a subset of LiteLLM's data) and is updated with new versions of Trilium. Local providers are considered free, whereas custom endpoint providers don't offer any pricing information.

## Features

### Web search

The AI can optionally search the web to find more information about a specific topic. 

This feature is on by default but it can easily be disabled by clicking on the model selector at the bottom of the chat and unchecking _Web search_.

Currently only the search native to the LLM provider is supported. External search providers such as Exa, Tavily & SearXNG are not yet supported.

### Note access (tools)

Tools allow the agentic AI to understand and operate on notes directly within your Trilium instance.

This feature is on by default but it can easily be disabled by clicking on the model selector at the bottom of the chat and unchecking _Note access_.

Here are a few tools that Trilium provides for the LLM:

  * At note level: 
    * Search for notes
    * Get the metadata or content of a note.
    * Edit a note 
      * There are multiple mechanism for the LLM to edit a note: completely by re-writing it, find/replace of a text sequence or append.
      * Whenever the AI makes a change, a [revision](#root/_help_vZWERwf8U3nx) is saved to be able to revert any unwanted changes.
    * Create a new note
    * Rename or delete a note.
  * At attribute level: 
    * Get the full list of attributes, or a specific attribute.
    * Set the value of an attribute.
    * Delete an attribute.
  * At tree level: 
    * Get the direct children of a note.
    * Get the entire subtree of a note.
    * Move or clone a note somewhere else.
  * For [Attachments](#root/_help_0vhv7lsOLy82): 
    * Get metadata for an attachment.
    * Get the content of an attachment.
  * Skills (see the dedicated section).

Currently there is **no permission management** implemented for note tools, meaning that the LLM could potentially remove existing notes or clutter the tree with notes. Generally most actions are easily reversible (deleting the notes, restoring deleted notes, reverting modifications to a note), but there are some that are harder to revert (e.g. setting an attribute because there is no attribute history).

Gemini has a special case in which _Note access_ and _Web search_ can't be both enabled at the same time.

### Attachments

Since Trilium v0.140.0, [Attachments](#root/_help_0vhv7lsOLy82) allow for multi-modal chat:

  * Raster image (sent as vision input, except for SVGs) with the following supported formats: PNG, JPEG, GIF, WebP.
  * PDFs, sent natively to the provider (supported by Anthropic, OpenAI and Google).
  * SVG images (sent as raw HTML).
  * Text files.

To upload an attachment:

  * Press the dedicated _Attach_ button (paperclip icon) underneath the text box.
  * Paste an image directly from clipboard using Ctrl+V.

Once one or more attachments are uploaded, they will appear directly above the text box:

  * Images have a small thumbnail for easy identifications.
  * Every attachment can be deleted by pressing their corresponding X button.

When an attachment is present, the LLM is instructed to consider the attachment with priority, even if it has access to the current note.

Currently Trilium doesn't pre-process the attachments (e.g. via  [Text Extraction (OCR)](#root/_help_TiQbQDgP8L5t)) before sending them to the LLM provider.

### Mentions

Mentions are a way to insert references to notes other than the current note, using the same mechanism as [Internal (reference) links](#root/_help_hrZ1D00cLbal). To refer to another note, simply type @ followed by the name of the note to reference.

This feature is mostly helpful when note tools are enabled, otherwise the LLM will have no way to access the given note.

### Skills

Skills in Trilium are specialized instruction sets that help the AI be more productive by understanding how Trilium.

These skills are not loaded by default to avoid an increased consumption of tokens, but the AI can load them on-demand if _Note tools_ are enabled.

The following skills are built-in:

  * Search syntax: understands the full syntax of [Search](#root/_help_eIg8jdvaoNNd).
  * Backend scripting: to be able to write proper [Backend scripts](#root/_help_SPirpZypehBG).
  * Frontend scripting: to be able to write proper [front-end scripts](#root/_help_yIhgI5H7A2Sm) (basic scripts, widgets, [Render Note](#root/_help_HcABDtFCkbFN)).

When _Note tools_ are enabled the skills will automatically be made available to the AI, so no user interaction is required.

Custom skills are currently not supported but they are planned.

### MCP

Trilium comes with a built-in MCP server which allows you to use an external agent such as Claude Code have access to your database. See the dedicated  [MCP](#root/_help_KOWBFgsZXJAD) page for more details.

## History

### Removal in v0.102.0

Starting with version v0.102.0, AI/LLM integration has been removed from the Trilium Notes core.

While a significant amount of effort went into developing this feature, maintaining and supporting it long-term proved to be unsustainable.

When upgrading to v0.102.0, your Chat notes will be preserved, but instead of the dedicated chat window they will be turned to a normal [Code](#root/_help_6f9hih2hXXZk) note, revealing the underlying JSON of the conversation.

### Reintroduction in v0.103.0

Given the recent advancements of the AI scene, we decided to give the LLM integration another try. v0.103.0 introduces a completely new chat system.

One of the key changes that lead to the reimplementation is that now we are using a library ([Vercel AI](https://github.com/vercel/ai)) to manage the inner mechanism and the differences between LLM providers instead of having to implement it on our own.
