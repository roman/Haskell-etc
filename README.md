# etc

Etc is a library that helps gathering configuration values for a system from
multiple sources (opt-parsers, env vars, files) by using data declarations on a
JSON configuration file.

To get started you'll need a `spec.js` file where this configuration values are
declared:

```json
{
  "spec/filepaths": ["./config/app.json",
                     "/etc/app/config.json"],
  "spec/entries": {

  }
}
```

## Configuration Value Resolvers

`etc` provides 3 resolvers to get configuration values from:

### Filepaths

### ENV Vars

### OptParser