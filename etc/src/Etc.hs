{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|

The @etc@ library allows you to manage the configuration values of your
application in a hassle free and non-error prone way. With this library you can:

* Maintain validated documentation of the configuration entries of your
  application via the configuration spec file

* Add new "input sources" for values of your configuration entries without
  modifying your core business code

* Have compilation errors when your configuration spec file is incorrect

* Have compilation errors when accessing configuration values that are unknown

* Fail as soon as possible when configuration files have invalid values or
  invalid entries

* Decouple your application logic from where configuration values come from, you
  no longer need to use @IO@ in the middle of your core business code to get the
  value of an Environment Variable.

* Offer state of the art error reports

* Audit where configuration values come from when dealing with multiple input
  sources

* Keep sensitive configuration values (like passwords) hidden when performing
  auditing of the configuration of your app

To use this library you need to understand three basic APIs:

* 'Spec.ConfigSpec' which represents your configuration spec file at runtime

* 'Resolver.Resolver' which translates configuration entry metadata into runtime values your app needs

* 'Config.Config' where the runtime value information is going to be queried from

Following is a deep-dive into how to use the essential functions to get using
@etc@ effectively.

@since 1.0.0.0
-}
module Etc
  (
    -- * ConfigSpec API
    -- $configuration_spec_api_intro

    -- ** Basic usage
    -- $configuration_spec_basic_usage
    Spec.ConfigSpec
  , Spec.readConfigSpecTH
  , Spec.readConfigSpecFormatTH
  , Spec.parseConfigSpec
  , Spec.yamlSpec
  , Spec.jsonSpec

    -- ** Use your own types
    -- $configuration_spec_custom_types
  , Spec.CustomType
  , CustomType.aesonCustomType
  , CustomType.textCustomType
  , CustomType.boundedIntCustomType
  , CustomType.boundedFloatCustomType

    -- * Resolver API
    -- $resolver_api_intro

    -- ** Basic Usage
    -- $resolver_usage
  , Resolver.Resolver
  , Resolver.resolveConfig
  , Resolver.resolveConfigWith

    -- ** File Resolver
    -- $resolver_api_file
  , File.fileResolver
  , File.jsonConfig
  , File.yamlConfig

    -- ** Environment Variable Resolver
    -- $resolver_api_envvar
  , Env.envResolver
  , Env.pureEnvResolver

    -- * Config API
    -- $config_api
  , Config.Config
  , Config.HasConfig (..)
  , Config.ConfigValueParserFailed(..)
  , Config.InvalidConfigKeyPath(..)
  , Config.getConfigValue
  , Config.getConfigValueWith
  , Config.checkedConfigValue

  ) where

import qualified Etc.Internal.Config as Config
import qualified Etc.Internal.Config.TH as Config
import qualified Etc.Internal.CustomType as CustomType
-- import qualified Etc.Internal.FileFormat as FileFormat
import qualified Etc.Internal.Spec.Parser as Spec
import qualified Etc.Internal.Spec.Types as Spec
import qualified Etc.Internal.Resolver as Resolver
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Resolver.File as File
import qualified Etc.Internal.Resolver.Env as Env

{- $configuration_spec_api_intro

A 'ConfigSpec' record is a validated in-memory representation of the
configuration spec file.

The configuration spec file is intended to contain __all__ metadata related to
your application configuration; information like:

* What are all the application configuration entries of my application?

* What are the default values of my application configuration entries?

* What are all the environment variables my application understands?

* Does any particular configuration entry can be read on an environment
  variable?

* What is the intended type of a configuration entry?

Because all this metadata is contained on a single file, it can be versioned
with the rest of your code, allowing you to keep track on how your configuration
evolves over time; not just that, it allows the @etc@ library to validate and
sanity check that all your configuration entries make sense. Some examples:

* Report an error when configuration files contain unknown entries; this
  commonly happens when refactoring your configuration names

* Report when there is a typo on a runtime environment variable; because @etc@
  knows all environment variables your application will use, it can go through
  all the OS variables and track if there is an "almost matching" entry

* Report when a configuration file entry, or an environment variable contains an
  invalid value.

When @etc@ finds inconsistencies in your configuration, it fails as soon as
posible with user friendly and actionable error messages.

-}

{- $configuration_spec_basic_usage

The best way to get started is to use the 'Spec.readConfigSpecTH' function. This by default
will read and parse a YAML configuration file at compilation time.

If for some reason, you want to maintain a configuration spec file in JSON, you
may use the 'Spec.readConfigSpecFormatTH' function which receives a
'FileFormat.FileFormat' argument. The available file formats are
'FileFormat.yamlSpec' and 'FileFormat.jsonSpec'.

-}

{- $configuration_spec_custom_types

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

-}


{- $resolver_api_intro

A 'Resolver' is an interface that wraps some logic that may parse and interpret
the contents of a configuration spec. The 'Resolver' is responsible of gathering
a configuration value from a particular source, some examples are:

* Configuration Files
* Environment Variables
* Command Line Options
* Hashicorp Vault entries
* AWS Parameter Store entries
* etc.

This library comes with out-of-the-box resolvers for Configuration Files and
Environment Variables, however, the library is extensible enough to allow other
hackage libraries to offer other resolvers (imagine: @etc-cli@, @etc-vault@,
etc.)

The advantage of having different resolvers is that you can pick and choose
which resolver makes sense for your application. Say for example you are not
using CLI opt parsing in your program, you can skip that dependency entirely by
not including @etc-cli@ in your project's cabal file.

-}

{- $resolver_usage

For most applications out there, developers are happy with configuration files
and environment variables as the source for their application configuration.
Given this, @etc@ offers the basic function 'Resolver.resolveConfig' that gives
you just that. If you need to gather the configuration values of your
application from other "input sources" besides the ones mentioned above, you may
want to check the 'Resolver.resolveConfigWith' function.

-}

{- $resolver_api_file

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

-}

{- $resolver_api_envvar

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

-}

{- $config_api

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

-}
