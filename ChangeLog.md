# v0.10.0 - 27/11/2026

- Restore the API of v0.8.0, broken by v0.9.0: the `sel` library is again
  the Unix one, and its module `Sel` has all the events, including
  `Sel.On.line`, `Sel.On.bytes`, `Sel.On.httpcle`, `Sel.On.ocaml_value` and
  `Sel.On.death_of`.
- The system agnostic part is the virtual library `sel.core`, module
  `Sel_core`, implemented by `sel` (the default) and `sel.js`. `Sel` includes
  `Sel_core`, the types are the same. Libraries only creating events can
  depend on `sel.core` and work with both implementations.
- The `sel.unix` library and the `Sel_unix` module are gone, use `sel` and
  `Sel`. Programs using `sel.js` use `Sel_core` in place of `Sel`.

# v0.9.0 - 26/11/2026

- `sel` is a virtual library, with two implementations: `sel.unix` and `sel.js`.
  Libraries only creating events (queues, promises, `Sel.now`) can depend
  on `sel` and work with both. They are all in the `sel` opam package.
  - `sel.unix`, the default implementation, has a backend based on
    `Unix.select`, and its module `Sel_unix` contains events `Sel_unix.On.line`,
    `Sel_unix.On.bytes`, `Sel_unix.On.httpcle`, `Sel_unix.On.ocaml_value`,
    `Sel_unix.On.death_of`.
  - the optional `sel.js`, built if `js_of_ocaml` is installed, is for
    programs compiled with js_of_ocaml or wasm_of_ocaml.
    Its module `Sel_js` provides a driver,
    `Sel_js.run ~handle_event ~init ()`, that handles the ready events from
    the JavaScript event loop, and adds the events returned by
    `handle_event` to its todo. `Sel_js.wakeup` is to be called when a
    JavaScript callback unblocks an event (eg feeds a queue)
- New non blocking APIs `pop_ready` and `wait_ready`, to integrate `sel` in
  an external event loop
- Promises do not use a pipe-per-promise anymore, the Unix backend uses a self
  pipe to wake up `select` when a promise is resolved
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
