0.5.0
=====

  * Switched to `network-uri`

  * Removed `io` from the API. Quick migration guide: `io` -> `liftIO`

0.4.3.0
=======

  * Upgraded dependencies' bounds

0.4.2.0
=======

  * Generalized `ConnectInfo`

0.4.1.0
=======

  * Generalized `io`

  * Added `runJenkinsThrowing`

0.4.0.0
=======

  * Switched to `Text` for username and API token (password)

  * Supported `lens-4.0`

0.3.0.0
=======

  * `restart` does not send requests to `$jenkins_url/restart` anymore. Instead, it calls
  `$jenkins_url/safe-restart` which waits running jobs to complete. New `forceRestart` function
  does now what `restart` did before

  * Massive refactoring

  * More optics in `Network.HTTP.Conduit.Lens`

  * Added `overallLoad` and `computer` REST API methods shortcuts

0.2.0.0
=======

  * Moved onto http-conduit 2.0 API.

0.1.0.0
=======

  * Initial release. REST and Discovery APIs support.
