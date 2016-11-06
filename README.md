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

This project uses stack for building, so you just need to run:

```bash
$ stack build
```

To run the example, you can start it from GHCi.

```bash
$ stack repl
...
λ main
```

Or you can build the project and run the executable.

```bash
$ stack build
$ stack exec example
```

Depending On `reflex-websockets`
---

Until `reflex-websockets` and the newer builds of `reflex` make their
way to Hackage, you'll have to depend on them both from GitHub, as
shown below. You'll need to set the commit to whatever the `master`
branch is at, and you'll need to specify the corresponding `reflex`
commit to be the same one used by `reflex-websockets`. Of course, you
may change these at your discretion.

```yaml
packages:
- '.'
- location:
    git: https://github.com/ElvishJerricco/reflex-websockets
    commit: ...
  subdirs:
  - reflex-websockets
- location:
    git: https://github.com/reflex-frp/reflex
    commit: ...
```
