Multi index is an erlang module that implements a key - value dictionary which
stores values based on multiple keys. Multi index only stores values, the keys
used for indices are derived from the values by user defined key functions.
There can be multiple indices, and it is possible to look up values based on
any of these indices. A common use case is to store tuples, which can be looked
up by each of their elements.

See the html documentation [here](http://grogers0.github.com/erl_multi_index/multi_index.html).
