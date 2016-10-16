# etc

Etc helps gathering configuration values for a system from multiple sources
(opt-parsers, env vars, files) in a declarative fashion using a JSON
configuration file.

To get started you'll need a JSON file where this configuration values are
declared, say for example you have a file on your project called
`resources/spec.json`:

```json
{
  "etc/filepaths": ["./resources/app.json",
                     "/etc/app/config.json"],
  "etc/optparse": {
    "desc": "Description of the program that reads this configuration spec",
    "header": "my-app - A program that has declarative configuration input",
    "commands": {
      "config": {
        "desc": "Prints configuration summary"
      },
      "run": {
        "desc": "Executes main program"
      }
    }
  },

  "etc/entries": {
    "credentials": {
      "username": {
        "etc/spec": {
          "default": "root",
          "env": "MY_APP_USERNAME",
          "optparse": {
            "input": "option",
            "metavar": "USERNAME",
            "help": "Username of the system",
            "type": "string",
            "required": false,
            "command": "run"
          }
        }
      },
      "password": {
        "etc/spec": {
          "env": "MY_APP_PASSWORD",
          "optparse": {
            "input": "option",
            "metavar": "PASSWORD",
            "help": "Password of user",
            "type": "string",
            "required": true,
            "command": "run"
          }
        }
      }
    }
  }
}
```

## Reading Configuration in Haskell Code

To read configuration values, there is 2 functions that can be used:

* `getConfigValue`

Reads values from a JSON configuration and it parses it
to Haskell data structures using the `Aeson.FromJSON` typeclass

* `getConfigValueWith`

Reads values from a JSON configuration and it parses it using a custom function
that uses the `Aeson` parser API; this works great when the configuration data
structures of libraries you may use don't support `Aeson` or the format in your
config file is not quite the same as the already implemented `Aeson.FromJSON`
parser.

### Example

NOTE: This example uses the `spec.json` given in the intro section

```haskell
import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified System.Etc as Etc

data Credentials
  = Credentials { username :: Text
                , password :: Text }
  deriving (Show)

parseCredentials json =
  case json of
    JSON.Object object ->
      Credentilas
        <$> object .: "user"
        <*> object .: "password"

main :: IO ()
main = do
  configSpec <- Etc.readConfigSpec "resources/spec.json"
  config     <- Etc.resolveFiles configSpec

  -- Get individual entries (Uses Aeson.FromJSON)
  username <- Etc.getConfigValue ["credentials", "username"]

  -- Get the values with a JSON parser
  creds <- Etc.getConfigValueWith parseCredentials ["credentials"]

  print (username :: Text)
  print creds
```

## Configuration Value Resolvers

`etc` provides 3 different resolvers to get configuration values from:

### Filepaths

You may specify an `etc/filepaths` entry on the spec file, this will tell `etc`
to merge a list of JSON configuration from each path, the latter the filepath,
the more precedence it has.

In the example given on the intro section, we have two configuration files

This helps to have a scheme of over-writable configurations on deployed
applications, say the first path in the list is the config file used on
development, and when you deploy your app it adds another configuration file on
a well known path (say `/etc` path) which has more precedence and overwrites
the development values.

### ENV Vars

### OptParser
