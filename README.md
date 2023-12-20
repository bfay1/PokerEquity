# Poker Equity Calculator

We built this with `stack build`, but we always compile with:
`stack ghc -- --make -threaded -Wall -eventlog -rtsopts Main.hs`

If you want to profile, you can compile it with :
`stack ghc -- --make -threaded -Wall -eventlog -rtsopts -fprof-auto -prof Main.hs`

To run it:
`./Main <suit> <rank> <suit> <rank> <numexperiments> <numplayers>
<parallelization>`

Where `<suit>` and `<rank>` represent the suits and ranks of the hand you are
trying to calculate, `<numexperiments>` is the number of experiments, and
`<numplayers>` is the number of players.

`<parallelization>` should be one of the following:

`sequential`
`naive`
`chunk`
`recursive`
`recursiveChunk`

which correspond to methods of parallelization that we use.

All of our benchmarking was done on Ubuntu 22.04 on a Lenovo Thinkpad X1 Carbon
10th generation.
