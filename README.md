[![Build Status](https://travis-ci.org/protofy/erl_protofy_common.svg)](https://travis-ci.org/protofy/erl_protofy_common)

protofy_common
==============

Some common erlang modules, types and macros used by protofy.

common_types.hrl
-----------------
Some types that help writing specs more readable. E.g. reason(), void(), ignored().


protofy_types.hrl
-----------------
Types used in this package.


protofy_macros.hrl
------------------
Well... some macros


protofy_common.hrl
------------------
Includes common_types.hrl, protofy_types.hrl, protofy_macros.hrl 


protofy_node
------------
Configure the current node.

configure/0, configure/1 configure the current node according to an app environment or a given proplist. They currently only set the cookie.

set_cookie/1, set_cookie/2 allow setting of cookies by atom, list, binary or file.


protofy_pulse
-------------
Generate and convert a pulse.

If you don't know what the pulse is, never mind. Will blog about it later.


protofy_time
------------
Time measurement and conversion functions.


protofy_inet
------------
Some inet helpers.


protofy_test_util
-----------------
Some utils to help writing tests.


Further Information
--------------------
CI with travis: https://travis-ci.org/Protofy/protofy_common

See modules for further information.

