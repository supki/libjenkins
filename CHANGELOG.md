0.5.0.0
=======

  * Generalize `io`

0.4.0.0
=======

  * Use `Text` for username and API token (password)

  * Support `lens-4.0`

0.3.0.0
=======

  * `restart` does not send requests to `$jenkins_url/restart` anymore. Instead, it calls
  `$jenkins_url/safe-restart` which waits running jobs to complete. New `forceRestart` function
  does now what `restart` did before

  * Massive refactoring

  * More optics in `Network.HTTP.Conduit.Lens`

  * Add `overallLoad` and `computer` REST API methods shortcuts

0.2.0.0
=======

  * Move onto http-conduit 2.0 API.

0.1.0.0
=======

  * Initial release. REST and Discovery APIs support.
