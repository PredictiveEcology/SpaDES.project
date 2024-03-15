Known issues: <https://github.com/PredictiveEcology/SpaDES.project/issues>

version 0.0.9
=============
* `Restart` argument of `setupProject` now uses better `rstudioapi` calls to open new project with active file.


version 0.0.8
=============

## Enhancements
* overhaul of all internals
* `setupProject` is a new omnibus function to address a number of needs for a SpaDES project, 
including `paths`, `modules`, `options`, and others. See `?setupProject`

## deprecations
* `newProject` has been replaced by `setupProject`

version 0.0.4
=============

## Enhancements
* `newProject` can now take a vector of module names; if provided, these will be downloaded to the `modulePath` directory.

version 0.0.2
=============

* move `SpaDES.core` from `Imports` to `Suggests`.

version 0.0.1
=============

* initial version
