`reflex-websockets`
---

This package lets you write websocket servers
using [reflex](https://github.com/reflex-frp/reflex).

Motivation
---

WebSocket applications tend to maintain some state for each
connection, which is naturally dynamically changing. As new
information comes in, it is common that you need to reactively respond
and send new information to any number of connected clients.
Functional reactive programming solves many of the issues in
applications with these needs, since you can easily react to
dynamically changing information.

Example
---

You can find an example of using this library in
the [example project](example/).

Building
---

This project uses nix for building, so you just need to run:

```bash
$ nix-build
```

You can also use cabal to do incremental development

```bash
$ nix-shell
$ cd example
$ cabal configure
$ cabal build
```

To run the example, you can start it from GHCi.

```bash
$ nix-shell
$ cd example
$ cabal repl
...
λ main
```

Or you can build the project and run the executable.

```bash
$ nix-build
$ result/bin/example

# Or

$ nix-shell
$ cd example
$ cabal run
```

Depending On `reflex-websockets`
---

TODO
