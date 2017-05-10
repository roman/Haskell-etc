# etc-plain-example

Similar to the [etc-plain-example]() app with the difference that this example
uses an embedded configuration spec in the source code via a Base64 encoding.

This example __requires__ the usage of the embedded Makefile in order to build
the binary application.

```bash
make prepare_config
```

For more information about `etc` API and how to use it, see
the [`etc` homepage](https://github.com/roman/Haskell-etc)
