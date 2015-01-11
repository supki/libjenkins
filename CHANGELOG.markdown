0.8.0
=====

  * Added `stream` to provide convenient streaming

  * Removed the support for disconnects, thus simplifying the public API slightly.
    Note that `reload`, `restard`, and `forceRestart` do not disconnect automatically anymore.

0.7.0
=====

  * Jenkins' responses to `POST` requests are being read again

  * Added `groovy` to ease the communication with the embedded Groovy
    interpreter (see example/repl.hs)

0.6.0
=====

  * Made `orElse` more powerful. The old version is available as `orElse_`

  * Renamed a bunch of things.  The `Jenkins.Rest` module is intended to be imported qualified.

  * Switched to the transformer version of the Church-encoded free monad

  * Removed `getS`. As a side-effect, `get` doesn't leak like crazy anymore

  * Generalized `traverseC_` (again)

  * Removed redundant `jenkinsPort` option: `jenkinsUrl` handles port numbers well enough

  * Reworked API method construction. The new version is safer (it's impossible to forget
    to specify the format of the response), less magical (format is a separate argument to
    the query function), and has fewer corner cases

0.5.0
=====

  * Replaced `concurrentlys` and `concurrentlys_` with `traverseC` and `traverseC_`
    respectively. Quick migration guide:

    + `concurrentlys`  -> `traverseC  id . toList`
    + `concurrentlys_` -> `traverseC_ id . toList`

  * Added `getS` for tighter control of Jenkins responses consumption

  * `post` variants do not read the response body at all anymore

  * Added `orElse`

  * Removed `runJenkinsThrowing` from the API

  * `runJenkins` only catches exceptions thrown by the execution of `Jenkins` queries

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
