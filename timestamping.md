sign then distribute sigs doesn't work because you can't know if all the nodes got the sigs to verify before the timeout.

you need some way to recover from transactions not being included by a node. This is where each node can listen to each other's changefeed.

---

node receives transaction and timestamps it.
node sends transaction to other nodes which sign and include.
each node subscribes to other's feed, which is delayed by some agreed number of seconds.
based upon feed, 


