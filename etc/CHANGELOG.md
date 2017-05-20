0.2.0.0
----
* Move `Config` API to typeclass `IConfig`
* Add a `Setup.hs` file to every hachage repo (issue #5)
* Add example of a project with a config spec embedded in the binary

0.1.0.0
----
* Add support for null values on Default (issue #3)
* If cli cabal flag is false, have `parseConfigSpec` return `ConfigSpec ()`
  instead of ambiguous `FromJSON` value (issue #3)
* Bump aeson dependency to `<1.3`

0.0.0.2
----
* Rename System.Etc.Internal.Util module to System.Etc.Internal.Extra
* Rename cabal flag from printer to extra
* Add feature for Environment Variable misspelling reports
* Add misspelling reports to example projects
* Improvements on README

0.0.0.1
----
* Various changes to source code to comply with previous resolvers
  * Use Monoid instead of Semigroup
  * Remove unused imports/typeclasses
* Bump upper limits for aeson and vector
* Improve README.md typos
* Add CHANGELOG
