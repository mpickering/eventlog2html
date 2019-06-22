# eventlog2html

[Interactive Documentation](https://mpickering.github.io/eventlog2html/)

[![Build Status](https://travis-ci.org/mpickering/eventlog2html.svg?branch=master)](https://travis-ci.org/mpickering/eventlog2html)

`eventlog2html` is a tool to visualise eventlogs. In particular, it creates
interactive charts for the heap profiling information included in the
eventlog.

The [complete documentation](https://mpickering.github.io/eventlog2html/) contain
[interactive](https://mpickering.github.io/eventlog2html/examples/ghc.eventlog.html) [examples](https://mpickering.github.io/eventlog2html/examples/hie.eventlog.html) and complete usage information.

## How to use

The tool produces a static webpage to visualise the heap profile.
For an eventlog `program.eventlog`, a static page titled `program.eventlog.html`
will be created adjacent to the original file.

In order to use `eventlog2html` you first need an eventlog with heap profiling
samples.

Compile your program with `-prof`. In a cabal project, the easiest way to
do this is to set `profiling: True` in the `cabal.project` file.

Then, run your program with the normal profiling flags with an additional `-l`
flag. This will tell GHC to also emit the eventlog.

```
my-leaky-program +RTS -hy -l-au
```

In the current directory a file `my-leaky-program.eventlog` will be produced.
This is what you need to pass to `eventlog2html` to generate the profiling
graphs.

```
eventlog2html my-leaky-program.eventlog
```

Note: The `-l-au` suffix will result in a significantly smaller eventlog
as it will not include thread events. This makes a big difference for
multi-threaded applications.




