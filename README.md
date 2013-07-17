# async-plgd

Trying out Clojure's core.async library.

## Usage

Clone the [core.async](http://github.com/clojure/core.async) repo to a
convenient location. `cd` into the repo and run `lein install`.

Then clone this repo into a convenient location and `cd` into it.

Then open a repl, and at the prompt, type

```
(use :reload-all 'hoare.coroutines)
```

and run any of the test functions in src/hoare/coroutines.clj.

None of the functions from 3.5 REFORMAT onwards, which execute pipelined coroutines,
work reliably. I believe this indicates that a control channel must be threaded through
the pipeline, even through stateless routines such as COPY.

Or there may be a bug in core.async.

Or, most likely, I'm doing it wrong.

## License

Copyright © 2013 Alan Shaw

Distributed under the Eclipse Public License, the same as Clojure.
