# pms-ui-notification

`pms-ui-notification` is one of the internal packages that make up the [`pty-mcp-server`](https://github.com/phoityne/pty-mcp-server) project.  
It is responsible for defining structured data types and utilities for formatting notifications to be sent to the user interface layer.

In the context of a stdio-mode MCP server, this package specifically handles the construction and serialization of JSON-RPC **notification** objects to be written to `stdout`.  
It serves as the final output layer for event-style communications, allowing the server to proactively inform the client of updates such as tool list changes or background events.

---

## Package Structure
![Package Structure](https://raw.githubusercontent.com/phoityne/pms-ui-notification/main/docs/01_package_structure.png)
---

## Module Structure
![Module Structure](https://raw.githubusercontent.com/phoityne/pms-ui-notification/main/docs/02_module_structure.png)

---
