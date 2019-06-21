---
title: Home
---

# eventlog2html

eventlog2html is a tool to visualise event


## How to use

In order to use `eventlog2html` you first need an eventlog with heap profiling
samples.

Compile your program with `-prof`. In a cabal project, the easiest way to
do this is to set `profiling: True` in the `cabal.project` file.

Then, run your program with the normal profiling flags with an additional `-l`
flag. This will tell GHC to also emit the eventlog.

```
my-leaky-program +RTS -hy -l
```

In the current directory a file `my-leaky-program.eventlog` will be produced.
This is what you need to pass to `eventlog2html` to generate the profiling
graphs.

```{.eventlog}
ghc.eventlog
```



### Adding markers

## Implementation

## Examples


