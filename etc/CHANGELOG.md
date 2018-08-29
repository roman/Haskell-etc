0.4.1.0
----

* Add `parseConfigSpecTH` functionality, now we can parse the `ConfigSpec`
  record at compilation time (closes #47)
* Remove bug on CLI option parser, now it will coerce numbers and
  booleans to string when specifying numbers and booleans over CLI
  and the field type is a string (closes #48)
* Improve Error Types to be more granular and descriptive
* Re-organize `Spec` parser functions in its own module
* Add `switch` input for CLI spec (closes #41)


0.4.0.3
----

* Remove bug that stop configuration to be rendered when a default
  value was an empty array

0.4.0.2
----

* Improvement around unhelpful error being thrown when CLI Config Map didn't
  contain a value on a required (by code) field

0.4.0.1
----

* Improve pretty printer for configuration values (closes #32)

0.4.0.0
----
**BREAKING CHANGES**

* Add new `type` field to `etc/spec` with support for `string`, `number`,
  `bool`, `[string]`, `[number]` and `[bool]`, `[object]`
* Remove `type` field in `cli` spec in favor of `type` on `etc/spec`
* Allow ENV vars to accept supported types (only strings were allowed) (closes #30)
* Allow CLI options to accept supported types (only strings and numbers were allowed)
* Allow spec file to have array as default values
* Return a warning and an empty config whenever configuration files contain
  entries not defined in the spec (closes #26)

0.3.2.0
----

* Upgrade to `rio` to version `0.1.1.0`

0.3.1.0
----

* Add new `etc/files` entry to the spec file which allows to specify an
  environment variable to fetch a file


0.3.0.0
----
**BREAKING CHANGES**

* Bump resolver to lts-11
* Replace `protolude` in favor of `rio`
* Update `parseConfigSpec` to no longer attempt to parse JSON when `yaml` cabal
   flag is set
* Add `sensitive` setting to `etc/spec` entries
* Add `Value` type to deal with sensitive values
* Update examples with `sensitive` values
* Add optional key context to the `InvalidConfiguration` error (issue #12)
* Add config printer function that renders config map with/without colors (issue #15)
* Give precedence to values that do not contain inner `JSON.Null` values (issue #16)

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
