# etc-embedded-config-example

Similar to the [etc-plain-example]() app with the difference that this example
uses an embedded configuration spec in the source code via a Base64 encoding.

This example __requires__ the execution of the `prepare_config` task from this
project's Makefile to build the binary application succinctly. Execute `make`
to see options.

For more information about `etc` API and how to use it, see
the [`etc` homepage](https://github.com/roman/Haskell-etc)
