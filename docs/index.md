---
title: eventlog2html
---

`eventlog2html` is a tool for creating interactive visualisations of eventlogs. In particular, it creates
interactive charts from the heap profiling events.

```{.eventlog traces=False }
examples/ghc.eventlog --bands 10
```

The default view is a stacked area chart which shows the cumulative memory
usage of a program over time with a band for each type of allocation.
This is similar to the chart produced by `hp2ps` and `hp2pretty`.

This chart is interactive. An area can be selected on the viewfinder, the
selection can be scrolled and zoomed. The legend is interactive, click on the
toggles to select individual bands to display and shift-click to select multiple
bands to display at once.

## How to use

`eventlog2html` is released on [hackage](https://hackage.haskell.org/package/eventlog2html) and can be installed like any normal Haskell executable.

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


<div class="alert alert-info" role="alert">

The `-l-au` suffix will result in a significantly smaller eventlog
as it will not include thread events. This makes a big difference for
multi-threaded applications.

</div>

### Adding markers

An advantage of using the eventlog is that other events can be correlated with
heap allocations. Strings emitted using [`traceMarker`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Debug-Trace.html#v:traceMarker) are displayed on the
chart as a verticle gray line. Hovering near the line will display the name of
the event which is nearest to the cursor.

```{.eventlog traces=True }
examples/ghc.eventlog --bands 10 --include-trace-events
```

### Normalised Line Chart

The normalised line chart is useful for finding out what is increasing in
memory throughout the profiling run. Each line represents one type of allocation
and the values are normalised against the maximum allocation value for that
type. A monotonically increasing line means an increasing amount of memory is
being used. A constant line means a constant amount of memory is being used.
This information can be hard to see in the stacked charts.

```{.eventlog type=line}
examples/ghc.eventlog --bands 10 --include-trace-events
```

## FAQ

### What about biographical and retainer profiling?

The eventlog will start supporting these two profiling modes in GHC 8.10.

`eventlog2html` can still read the old `.hp` file format. Pass the
`--heap-profile` flag to interpret the argument as as `.hp` file rather than
an eventlog.

### What if I want to do my own analysis on the heap profile?

`eventlog2html` can output JSON if you pass the `--json` flag. The information
is a list of rows. One entry for each heap profiling event. Remember to pass
`--bands 0` if you want the whole data rather than just the 15 top allocators.

### How can I filter the number of traces?

There are three options for controlling the display of traces on the chart.

* `--no-traces` will remove all traces from the chart.
* `-i SUBSTRING` will keep traces which contain the given substring.
* `-x SUBSTRING` will remove traces which contain a given substring.

If a trace matches both an `-i` and an `-x` option then it is included in the
chart.

### Why are the html files so big?

We bundle all the javascript dependencies so the heap profiles can be
viewed offline. The `--no-include-js` option will replace the bundled dependencies with links to a CDN if you prefer a smaller file size.

## Implementation

The charts are implemented using the [`vega-lite`](https://vega.github.io/vega-lite/) library. If there's a visualisation that you would like to see added or a
different type of chart then feel free to open an issue.

## Development

The library started as a fork of [`hp2pretty`](https://hackage.haskell.org/package/hp2pretty) but the majority of code has been rewritten since then. It was
implemented by Matthew Pickering of this parish and [David Binder](https://github.com/BinderDavid) around the time of Zurihac 2019.
The project is developed on [GitHub](https://github.com/mpickering/eventlog2html).

## Complete Options

```{.help}
```




