# Fastchain

This is a toy POC using a Postgresql database as a data backend.

Check out [bitcoin.sql](./bitcoin.sql) to get a flavour of what writing a fastchain application looks like.

Write queries are sent to the replication protocol before being applied to the database.

The consensus protocol sucks, it uses wallclock timestamps.

It was nearly possible, but not quite, to configure an SQL environment against sources of volatility :). This is more about securing against programmers doing things they shouldn't than any real adversary.

Enjoy!
