-- HLint configuration file

module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- Protolude removes the String alias to avoid its usage
ignore "Use String"
-- Because we need underscored names for test discovery
-- this hint isn't much use any more.
ignore "Use camelCase" =
  System.Etc.SpecTest System.Etc.Resolver.Cli.PlainTest System.Etc.Resolver.Cli.CommandTest
-- Reduce code duplication on tests is false economy
ignore "Reduce duplication" =
  System.Etc.Resolver.DefaultTest System.Etc.Resolver.Cli.CommandTest
-- CPP Pragmas makes this rather necessary
ignore "Redundant do" = System.Etc.Resolver.FileTest.tests
