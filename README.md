# eventlog2html

[Interactive Documentation](https://mpickering.github.io/eventlog2html/)

[![Build Status](https://travis-ci.org/mpickering/eventlog2html.svg?branch=master)](https://travis-ci.org/mpickering/eventlog2html)
![Hackage](https://img.shields.io/hackage/v/eventlog2html.svg)

`eventlog2html` is a tool to visualise eventlogs. In particular, it creates
interactive charts for the heap profiling information included in the
eventlog.

The [complete documentation](https://mpickering.github.io/eventlog2html/) contain
[interactive](https://mpickering.github.io/eventlog2html/examples/ghc.eventlog.html) [examples](https://mpickering.github.io/eventlog2html/examples/eventlog2html.eventlog.html) and complete usage information.

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

## Building the project

There are two supported ways to build the project: `cabal new-build` and `nix`.
For development the normal `cabal new-build` workflow should work fine.

There are also nix files provided which can build both the tool and documentation.
Binary caches are populated by CI.

```
cachix use mpickering
nix build -f . eventlog2html
nix build -f . site
```

Using `nix` means that you can conveniently try the project using

```
nix run -f https://github.com/mpickering/eventlog2html/archive/master.tar.gz eventlog2html -c eventlog2html my-leaky-program.eventlog
```

## Fixing CI

If we need a newer version of a dependency then it might be necessary to update
the `index-state` which `haskell.nix` uses to compute the build plan.

All this requires is updating the date in `build.nix` to something more recent.
The `index-state-hashes` are updated once a day so you might have to choose a
date from a few days ago rather than the current date.

## Modifying Documentation

The documentation for the project is located in the `docs` folder. It is a hakyll
site built by the generator in the `hakyll-eventlog` folder. CI automatically
builds and deploys changes to the documentation.

Additional examples can be added by adding an eventlog to the `examples`
subdirectory. They will automatically be added to the examples gallery.

You can build the documentation using the `nix` target or using `cabal
new-build hakyll-eventlog` and invoking the resulting executable in the `docs/`
directory.

There is a custom pandoc filter which can insert rendered eventlogs into the
documentation. To understand how this works it's probably easiest to read the code
or copy the existing examples.
