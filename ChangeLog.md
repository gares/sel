# Unreleased

- **Breaking**: `sel` is a virtual library, with two implementations: a
  program picks one by linking it, and gets `sel.unix` if it does not.
  Libraries only creating events (queues, promises, `Sel.now`) can depend
  on `sel` and work with both. They are all in the `sel` opam package.
  Modules are not wrapped anymore: `sel` provides the module `Sel`, and its
  internal modules are named `sel__*`
  - `sel.unix`, the default implementation, has a backend based on
    `Unix.select`, and its module `Sel_unix` the Unix specific events, that
    were in `Sel.On`: `Sel_unix.On.line`, `Sel_unix.On.bytes`,
    `Sel_unix.On.httpcle`, `Sel_unix.On.ocaml_value`,
    `Sel_unix.On.death_of`. A native program only needs to replace
    `Sel.On.line` with `Sel_unix.On.line` (and so on)
  - the optional `sel.js`, built if `js_of_ocaml` is installed, is for
    programs compiled with js_of_ocaml or wasm_of_ocaml. Its backend never
    blocks (`Sel.pop` and `Sel.wait` fail if no event is ready), uses
    `Date.now` as a clock and wakes up the driver when a promise is
    resolved. Its module `Sel_js` provides a driver,
    `Sel_js.run ~handle_event ~init ()`, that handles the ready events from
    the JavaScript event loop, and adds the events returned by
    `handle_event` to its todo. There is one driver: `run` fails if called
    twice. `Sel_js.wakeup` is to be called when a JavaScript callback
    feeds a queue
- New non blocking APIs `pop_ready` and `wait_ready`, to integrate `sel` in
  an external event loop
- New extension point `Sel.System_event` to implement system events
- Promises do not use a pipe anymore, the Unix backend uses a self pipe to
  wake up `select` when a promise is resolved
- Requires OCaml >= 4.12 (Atomic) and dune >= 3.17

# v0.8.0 - 2/12/2025

- New parameter `~undup` to `Sel.now` to avoid duplicate events

# v0.7.0 - 13/06/2025

- New API Sel.On.promise (fullfillment or rejection)

# v0.6.0 - 12/02/2025

- New API Sel.On.queue_all getting all available queue items

# v0.5.0 - 07/11/2024

- Resilient to multiple copies of the same event on the same file descriptor
- A system event is given enough rounds to complete if his priority is the lowest

# v0.4.0 - 03/7/2023

- Bugfix in event postponement (API `pop`)

# v0.3.0 - 02/7/2023

- Major code cleanup and documentation
- Removal of recurrent events
- APIs `pop` and `wait` can now be freely mixed
- CI on Linux, Windows and MacOS

# v0.2.0 - 31/5/2023

- Major API cleanup and documentation

# v0.1.0 - 26/5/2023

- First release
