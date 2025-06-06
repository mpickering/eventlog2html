0.12.0 release 2024-06-03
-------------------------

* Fix rendering of .hp profiles (#191)
* Add support for eras profiling

0.11.1 release 2024-08-17
-------------------------

* Fix build error due to new exported function in blaze-html [#186](https://github.com/mpickering/eventlog2html/issues/186)
* Add `GHC 9.10.1` to the build matrix.

0.11.0 release 2024-02-06
-------------------------

* Add sorting by numbers [#183](https://github.com/mpickering/eventlog2html/pull/183)
* Refactoring and modernization of the UI [#178](https://github.com/mpickering/eventlog2html/pull/178)
* Compatibility with `9.0`, `9.2`, `9.4`, `9.6` and `9.8` which are now also checked in CI.
  ([#180](https://github.com/mpickering/eventlog2html/pull/180) and [#181](https://github.com/mpickering/eventlog2html/pull/181))

0.10.0 release 2023-03-31
-------------------------

* Add support for rendering ticky profiles ([#167](https://github.com/mpickering/eventlog2html/pull/167))

0.9.3 release 2022-12-11
------------------------

* Add `--version` flag for eventlog2html which prints the version,
  git commit and git branch used to build eventlog2html.
* Compatibility with `9.4.*` and `9.2.*` compilers.

0.9.2 release 2021-11-24
------------------------

* Improve profile load speed ([#150](https://github.com/mpickering/eventlog2html/pull/150))

0.9.1 release 2021-06-21
------------------------

* Improve detailed mode for cost centre profiling ([#146](https://github.com/mpickering/eventlog2html/pull/146))

0.9 release 2021-03-17
----------------------

* Add support for `-hi` profiling
* Add a chart to show memory usage vs live bytes

0.8.3 release 2020-12-29
------------------------

* Add `defaultArgs` and `generateJsonData` for using eventlog2html as a library
* Update to `hvega-0.11`

0.8.2, release 2020-12-17
-------------------------

* Fix initialisation of fancyTable ([#137](https://github.com/mpickering/eventlog2html/pull/137))

0.8.1, release 2020-12-16
-------------------------

* Add a missing file to the cabal file to fix `0.8` release

0.8, release 2020-12-11
-----------------------

* Fix a bug for Windows users by explicitly setting output to utf-8. ([#123](https://github.com/mpickering/eventlog2html/issues/123))
* New detailed view to display per-band information
* Update vega assets to latest versions

0.7, released 2020-03-26
------------------------

* Use native interactive legend in new version of vega-lite
* Update to latest vega, vega-lite and vega-embed versions

0.6, released 2019-10-22
------------------------

* Revamp how cost centre profiles are displayed.
* Fix incorrectly calculated start time for certain profiles.
* Line chart now displays points for each sample so it's easier to see where to hover.
* Add `--y-axis` option to allow the user to specify the extent of the y-axis.
  This is useful when comparing two different profiles together.

0.5, released 2019-10-11
------------------------

* Add some more metainformation to the header (sample mode and sample interval)
* Fix empty sample at start of eventlog
* Support for biographical and retaining profiling modes if using
at least GHC-8.10.
* Fix cost centre profiles to match the output of hp2pretty

0.4, released 2019-09-18
------------------------

*  BREAKING CHANGE: eventlog2html no longer includes traces which have been generated by "traceEvent" or "traceEventIO" from "Debug.Trace" by default.
  "traceEvent" and "traceEventIO" are supposed to be used for high-frequency events.
  If you want to trace low-frequency events, especially in order to relate phases of your program to memory usage patterns,  use "traceMarker" and "traceMarkerIO" instead.
  If you want to return to the old behaviour, add the `--include-trace-events` option on the commandline.
* Removed "trace PERCENT" option, which had no effect in the code.
* Added warning about eventlogs with a lot of traces.
* Added option to filter the traces which should be included in the
  generated output.

0.3, released 2019-09-08
------------------------

* Added warnings if eventlog2html is used on eventlogs generated by GHC Version without profiling support.
* Moved to version `0.4` of HVega.
* HeapProfCostCentre and HeapProfSampleCostCentre Events are included  in the generated output.

0.2, released 2019-07-05
------------------------

* Added the commandline option `-o OUTFILE` which writes the output to
  the given filename.
* Show the time the eventlog was created in the generated HTML output.

0.1, released 2019-06-23
------------------------

* Initial release, a complete rewrite on hp2pretty. Implemented by
  Matthew Pickering and David Binder.
