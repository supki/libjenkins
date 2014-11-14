# Benchmarks

## `Concurrent.hs`

    ./bench/Concurrency (concurrent|sequential) HOST PORT USERNAME PASSWORD

Measures the concurrency impact on time API queries take.

Concurrency is the most useful if you:

  * Do not use any kind of "proxy" servers (apache2, nginx, etc) and _especially_ https
  * Try to run queries from the nearest machine possible: network latency matters

Under these conditions, expect to get ~2x speedup from using `concurrent` mode on Jenkins server with ~500 jobs.
