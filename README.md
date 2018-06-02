[![Build Status](https://travis-ci.org/roman/Haskell-etc.svg?branch=master)](https://travis-ci.org/roman/Haskell-etc)
[![CircleCI](https://circleci.com/gh/roman/Haskell-etc.svg?style=svg)](https://circleci.com/gh/roman/Haskell-etc)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-etc/v0.4.0.2.svg)](https://img.shields.io/github/commits-since/roman/haskell-etc/v0.4.0.2.svg)
[![Hackage](https://img.shields.io/hackage/v/etc.svg)](https://img.shields.io/hackage/v/etc.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/etc.svg)](https://img.shields.io/hackage/v/etc.svg)
[![Stackage LTS](http://stackage.org/package/etc/badge/lts)](http://stackage.org/lts/package/etc)
[![Stackage Nightly](http://stackage.org/package/etc/badge/nightly)](http://stackage.org/nightly/package/etc)

# etc

`etc` gathers configuration values from multiple sources (cli options, OS
environment variables, files) using a declarative spec file that defines where
these values are to be found and located in a configuration map.

## Table Of Contents

* [Raison d'etre](#raison-detre)
* [Defining a spec file](#defining-a-spec-file)
* [Reading a spec file](#reading-a-spec-file)
  * [YAML Support](#yaml-support)
* [Gathering configuration values explicitly](gathering-configuration-values-explicitly)
  * [Default](#default)
  * [Configuration Files](#configuration-files)
    * [Why have more than one configuration file?](why-have-more-than-one-configuration-file)
  * [Environment Variables](#environment-variables)
  * [Command Line](#command-line)
    * [`opt/cli` entries](#optcli-entries)
    * [`cli` entries](#cli-entries)
    * [Using Plain resolver](#using-plain-resolver)
    * [Using Command resolver](#using-command-resolver)
    * [CLI Support](#cli-support)
  * [Reading From Pure Sources](#reading-from-pure-sources)
* [Accessing configuration values](#accessing-configuration-values)
* [Printing your configuration values](#printing-your-configuration-values)
* [Report Misspellings on Environment Variables](#report-misspellings-on-environment-variables)
* [Cabal Flags](#cabal-flags)
* [Full Example](#full-example)


## Raison d'etre

`etc` is a configuration management that:

* Allows to have a versioned spec of all values your application can accept

* Provides documentation about Environment Variables used by your application

* Provides an API for gathering values from multiple sources (files, overwrite
  files, cli arguments, environment variables) and then composing them into a
  single configuration map

* Gives a sane precedence over sources of the configuration value sources

* Provides inspection utilities to understand why the configuration is the way
  it is

* Provides an API that abstracts away the source of configuration values and
  allows easy casting into record types your application or other libraries
  understand

## Defining a spec file

You need to use a spec file to define the structure of your application's
configuration map; also if an entry value on this configuration map can have
multiple input sources
([environment variable](#environment-variables),
[configuration files](#configuration-files),
[command line option](#command-line), etc), you can specify right there what
this sources may be. The map can be defined using JSON or [YAML](#yaml-support);
following an example in YAML format:

```yaml
###
# These paths are going to be read for configuration values merging values
# from all of them, if they have the same keys, entries on (2) will have
# precedence over entries on (1)
etc/filepaths:
- ./resources/config.json # 1
- /etc/my-app/config.json # 2

###
# The program is going to have a Command Line interface
etc/cli:
  desc: "Description of the program that reads this configuration spec"
  header: "my-app - A program that has declarative configuration input",

  # The program is going to have 2 sub-commands
  commands:
    config:
      desc: "Prints configuration summary"
      header: ""
    run:
      desc: "Executes main program"
      header: ""

###
# With etc/entries we define the configuration map structure your
# application is going to be reading values from
etc/entries:
  credentials:
    username:
      # Define the spec for ["credentials", "username"]
      etc/spec:
        # default value (least precedence)
        default: "root"

        # if environment variable is defined, put its value in this entry
        env: "MY_APP_USERNAME"

        # cli input is going to have one option for this value
        cli:
          input: option
          metavar: USERNAME
          help: Username of the system
          type: string
          required: false
          # option is going to be available only on run sub-command
          commands:
          - run

    # Define the spec for ["credentials", "password"]
    password:
      etc/spec:
        env: "MY_APP_PASSWORD"
        cli:
          input: option
          metavar: PASSWORD
          help: "Password of user"
          type: string
          required: true
          commands:
          - run
```

The important keys to notice on the previous example:

- `etc/filepaths` tells where to look for files to gather the configuration of
your app, it could be more than one file because you may want to have a default
file for development, and then override it with some configurations for
production/integration, The further the filepath is the higher precedence its
values are going to have.

- `etc/entries` specifies how your configuration map is going to look like and how
your business logic [will be accessing it](#accessing-configuration-values)

- `etc/spec` provide means to define metadata for a configuration value entry,
what is its default value, if it can be found via
an [environment variable](#environment-variables), or if it may be specified as
an [CLI option/argument](#command-line) input.

## Reading a spec file

To read a spec file you need to use the `System.Etc.readConfigSpec` function, this
function can accept either a JSON or YAML filepath. You can also use the
`System.Etc.parseConfigSpec` if you already gather the contents of a spec file from a
different source.

_NOTE_: When using `System.Etc.parseConfigSpec` or `System.Etc.readConfigSpec`
and the [CLI cabal feature flag is true](#cli-support), unless you use the
`System.Etc.resolveCommandCli` function, you will have to explicitly declare the
`ConfigSpec` type parameter.

### YAML support

In order to allow `etc` to read from YAML files, you will need to use the `yaml`
cabal flag when installing the library, here are some instructions on how to
pass cabal flags
using
[stack](https://docs.haskellstack.org/en/stable/nonstandard_project_init/?highlight=cabal%20flags#passing-flags-to-cabal) and
[cabal](http://stackoverflow.com/a/26490956/132987). We do this so that in case
you want to stick with the JSON format, you don't have to pull dependencies you
don't need.

## Gathering configuration values explicitly

Even though a spec file defines where the configuration values can be found,
`etc` won't collect those values unless it is explicitly told to do so. To do
this you must use functions that will _resolve_ these configuration sources.

### Default

When defining the spec, you can specify default values on the `etc/spec`
metadata entry. To get this values from the spec you must call the
`System.Etc.resolveDefault` with the result from `System.Etc.readConfigSpec` as an argument.

#### Example

```haskell
import qualified System.Etc as Etc

getConfiguration :: IO Etc.Config
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"
  return (Etc.resolveDefault spec)
```

### Configuration Files

To get values from configuration files on your filesystem, you must specify an
`etc/filepaths` entry on the spec file, this will tell `etc` to merge a list of
configuration values from each path, the latter the filepath, the more
precedence it has on the configuration map.

After this entry is defined in your spec, you must then call the
`System.Etc.resolveFiles` function with the result of `System.Etc.readConfigSpec` as a
parameter.

#### Why have more than one configuration file?

This helps to have a scheme of over-writable configurations on deployed
applications, you can have the first path in the list of `etc/filepaths` entry
be the config used while developing your app, and once deployed you can have
production configuration values on a well known path (say
`/etc/my-app/config.yaml`).

#### Example

```haskell
import Data.Monoid (mappend)
import qualified System.Etc as Etc

getConfiguration :: IO Etc.Config
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"

  let
    defaultConfig =
      Etc.resolveDefault spec

  (fileConfig, _fileWarnings) <- Etc.resolveFiles spec

  return (fileConfig `mappend` defaultConfig)
```

### Environment Variables

When an `env` key is specified in the `etc/spec` metadata of a configuration
value entry, `etc` will consider an environment variable with the given name.

After this entry is defined in your spec, you must then call the
`System.Etc.resolveEnv` function with the result of `System.Etc.readConfigSpec` as a
parameter.

#### Example

```haskell
import Data.Monoid (mappend)
import qualified System.Etc as Etc

getConfiguration :: IO Etc.Config
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"

  let
    defaultConfig =
      Etc.resolveDefault spec

  (fileConfig, _fileWarnings) <- Etc.resolveFiles spec
  envConfig  <- Etc.resolveEnv spec

  return (fileConfig `mappend` envConfig `mappend` defaultConfig)
```

### Command Line

You can setup a CLI input for your program by using the `etc/cli` entry at the
root of the spec file, and the `cli` entry on the `etc/spec` metadata entries
for configuration values.

When a `cli` key is specified in the `etc/spec` metadata of a configuration
value entry, `etc` will consider inputs from a command line interface for your
application.

#### `opt/cli` entries

The `opt/cli` entry map must have the following keys:

- `desc`: A one line description of what your application does

- `header`: The header used when getting the information from the auto-generated
  `--help` option

- `commands`: A map of sub-commands that this program can have; each entry is
   the name of the sub-command, and the value is a map with the key `desc` with
   the same purpose as the top-level `desc` entry defined above.

   *NOTE*: you must use [`System.Etc.resolveCommandCli`](#using-command-resolver) for
   the `commands` entry to take effect

#### `cli` entries

The `cli` entry map can have the following keys (`input` and `type` are
required):

- `required`: specifies if the entry is required on the CLI

- `input`: how you want to receive the input value, it can either be `argument`
  or `option`

- `type`: the type of the input value, this could be `string`, `number` or
  `switch` (only available on `option` inputs)

- `metavar`: the name of the input argument on the example/documentation string
  of the CLI help

- `long` (only available on `option` inputs): the name of the option in long
  form (e.g. `--name`)

- `short` (only available on `option` inputs): the name of the option in short
  form (.e.g `-n`)

- `commands`: A list of sub-commands that are going to have this option/argument
  available; make sure the commands listed here are also listed in the `etc/cli`
  entry of your spec file.

#### Using Plain resolver

When the `commands` key *is not* specified on the `etc/cli` entry of the spec
file, you *must* use this resolver.

After the `cli` entry is defined in your spec, you must then call the
`System.Etc.resolvePlainCli` function with the result of `System.Etc.readConfigSpec` as a
parameter.

##### Example

```haskell
import Data.Monoid (mappend)
import qualified System.Etc as Etc

getConfiguration :: IO Etc.Config
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"

  let
    defaultConfig =
      Etc.resolveDefault spec

  (fileConfig, _fileWarnings) <- Etc.resolveFiles spec
  envConfig  <- Etc.resolveEnv spec
  cliConfig  <- Etc.resolvePlainCli spec

  return (fileConfig
          `mappend` cliConfig
          `mappend` envConfig
          `mappend` defaultConfig)
```

#### Using Command resolver

When the `commands` key *is* specified on the `etc/cli` entry of the spec file, you *must*
use this resolver.

After the `cli` entry is defined in your spec, you must then call the
`System.Etc.resolveCommandCli` function with the result of `System.Etc.readConfigSpec` as a
parameter.

This will return a tuple with the chosen sub-command and the configuration map;
the command type needs to be an instance of the `Aeson.FromJSON`, `Aeson.ToJSON`
and `Data.Hashable.Hashable` typeclasses for the command to be parsed/serialized
effectively.

##### Example

```haskell
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import Data.Monoid (mappend)
import qualified System.Etc as Etc

data Cmd
  = Config
  | Run
  deriving (Show, Eq, Generic)

instance Hashable Cmd

instance JSON.FromJSON Cmd where
  parseJSON json =
    case json of
      JSON.String cmdName ->
        if cmdName == "config" then
          return Config
        else if cmdName == "run" then
          return Run
        else
          JSON.typeMismatch ("Cmd (" `mappend` Text.unpack cmdName `mappend` ")") json
      _ ->
        JSON.typeMismatch "Cmd" json

instance JSON.ToJSON Cmd where
  toJSON cmd =
    case cmd of
      Config ->
        JSON.String "config"
      Run ->
        JSON.String "run"

getConfiguration :: IO (Cmd, Etc.Config)
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"

  let
    defaultConfig =
      Etc.resolveDefault spec

  envConfig  <- Etc.resolveEnv spec
  (fileConfig, _fileWarnings) <- Etc.resolveFiles spec
  (cmd, cliConfig) <- Etc.resolveCommandCli spec

  return ( cmd
         , fileConfig
          `mappend` cliConfig
          `mappend` envConfig
          `mappend` defaultConfig)
```

### CLI Support

In order to allow `etc` to generate CLI inputs for your program, you will need
to use the `cli` cabal flag when installing the library, here are some
instructions on how to pass cabal flags
using
[stack](https://docs.haskellstack.org/en/stable/nonstandard_project_init/?highlight=cabal%20flags#passing-flags-to-cabal) and
[cabal](http://stackoverflow.com/a/26490956/132987). We do this so that in case
you are not interested in generating a CLI input for your program, you don't
have to pull dependencies you don't need.

### Reading from pure sources

Sometimes, you would like to use the concept of CLI or environment variables, without
actually calling the OS APIs, `etc` provides pure versions for these resolvers:

- `System.Etc.resolveEnvPure`

- `System.Etc.resolvePlainCliPure`

- `System.Etc.resolveCommandCliPure`

This work exactly the same as their non-pure counterparts, but receive one extra
argument to fetch the required input.

## Accessing Configuration Values

Internally, `etc` stores every value that it gathers from all sources like a
JSON object (using the `Data.Aeson.Value` type), this provides a lot of
flexibility around what value you can get from your configuration map, allowing
your to use Aeson typeclasses to cast configuration values to more business
logic data structures.

There are two functions that can be used to get values out from a configuration
map:

- `System.Etc.getConfigValue`

Reads values specified on a spec file and casts it to a Haskell type
using the `Aeson.FromJSON` typeclass

- `System.Etc.getConfigValueWith`

Reads values specified on a spec file and casts it using a custom function that
uses the `Aeson` parser API; this works great when the data structures of
libraries you use don't support `Aeson` or the format in your config file is not
quite the same as the already implemented `Aeson.FromJSON` parser of a type
given by a library.

An example of their usage is given in the [full example](#full-example) section

## Printing your configuration values

A lot of times you may want to assert where a configuration value is coming
from, or if a particular environment variable was considered effectively by your
program. You an use the `System.Etc.printPrettyConfig` function to render the
configuration map and the different values/sources that were resolved when
calculating it. This function is _really_ useful for debugging purposes.

### Example

Here is the output of one of
the
[example applications](https://github.com/roman/Haskell-etc/tree/master/etc-command-example):

```bash
$ MY_APP_USERNAME=foo etc-command-example run -u bar -p 123
Executing main program
credentials.username
  bar        [ Cli        ]
  foo        [ Env: MY_APP_USERNAME ]
  root       [ Default    ]

credentials.password
  123        [ Cli        ]
```

The output displays all the configuration values and their sources, the first
value on the list is the value that `System.Etc.getConfigValue` returns for that
particular key.

## Report Misspellings on Environment Variables

When you define `env` keys on the `etc/entries` map of your spec file, we can
infer what are the valid Environment Variables that need to be defined for your
application, knowing this, `etc` can infer when there is a typo on one of this
environment variables and report this. You need to have the `extra` cabal flag and
call the `System.Etc.reportEnvMisspellingWarnings` with the configuration spec as
as an argument.

### Example

Here is an example of the output this function prints to `stderr` when the given
Environment Variables are almost identical to the ones found on the spec file:

```bash
$ MY_AP_USERNAME=foo etc-command-example run -u bar -p 123

WARNING: Environment variable `MY_AP_USERNAME' found, perhaps you meant `MY_APP_USERNAME'

```

## Cabal Flags

To reduce the amount of dependencies this library brings, you can choose
the exact bits of functionality you need for your application.

- `yaml`: Allows (in addition of JSON) to have spec file and configuration files
  in YAML format

- `cli`: Provides the CLI functionality explained in this README

- `extra`: Provides helper functions for inspecting the resolved configuration
  as well as providing warning messages for misspelled environment variables

## Full Example

*NOTE*: This example uses the [spec file stated above](#defining-a-spec-file)

```haskell
import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.:))
import Data.Hashable (Hashable)
import Data.Monoid (mappend)
import GHC.Generics (Generic)

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified System.Etc as Etc

data Credentials
  = Credentials { username :: Text
                , password :: Text }
  deriving (Show)

data Cmd
  = Config
  | Run
  deriving (Show, Eq, Generic)

instance Hashable Cmd

instance JSON.FromJSON Cmd where
  parseJSON json =
    case json of
      JSON.String cmdName ->
        if cmdName == "config" then
          return Config
        else if cmdName == "run" then
          return Run
        else
          JSON.typeMismatch ("Cmd (" `mappend` Text.unpack cmdName `mappend` ")") json
      _ ->
        JSON.typeMismatch "Cmd" json

instance JSON.ToJSON Cmd where
  toJSON cmd =
    case cmd of
      Config ->
        JSON.String "config"
      Run ->
        JSON.String "run"

parseCredentials json =
  case json of
    JSON.Object object ->
      Credentials
        <$> object .: "user"
        <*> object .: "password"

getConfiguration :: IO (Cmd, Etc.Config)
getConfiguration = do
  spec <- Etc.readConfigSpec "./path/to/spec.yaml"

  Etc.reportEnvMisspellingWarnings spec

  let
    defaultConfig =
      Etc.resolveDefault spec

  (fileConfig, _fileWarnings) <- Etc.resolveFiles spec
  envConfig  <- Etc.resolveEnv spec
  (cmd, cliConfig) <- Etc.resolveCommandCli spec

  return ( cmd
         , fileConfig
          `mappend` cliConfig
          `mappend` envConfig
          `mappend` defaultConfig )

main :: IO ()
main = do
  (cmd, config) <- getConfiguration

  case cmd of
    Config -> do
      Etc.printPrettyConfig config

    Run -> do
      -- Get individual entries (Uses instance of Text type for the Aeson.FromJSON
      -- typeclass)
      username <- Etc.getConfigValue ["credentials", "username"]

      -- Get the values with a supplied JSON parser
      creds <- Etc.getConfigValueWith parseCredentials ["credentials"]

      print (username :: Text)
      print creds
```
