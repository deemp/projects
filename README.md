# Nginx-Clickhouse

## The goal

* Create a program to parse a log file based on the log format

## Run the project

* Clone this repository and go there
```sh
git clone https://github.com/br4ch1st0chr0n3/nginx-clickhouse-hs
cd nginx-clickhouse-hs
```

* Build and run
```sh
stack build
stack run
```

## Workflow

1. Takes a mapping `column name` -> `database type` from [schema.yml](./files/schema.yml) (the yml-ed version of [schema.sql](./files/schema.sql)). This is an  unnecessary step, since for now, the parser types for log elements are chosen by magic words, as in the original [solution](https://github.com/mintance/nginx-clickhouse/blob/4d36a9dded1ed1f9c90f3e89987ffef4766cc9db/nginx/nginx.go#L29)

1. Takes a mapping `column name` -> `magic word` and log format from [nginx_config.yml](./files/nginx_config.yml). I changed the log format to avoid collisions of `\x22` with `"` when using `megaparsec`

1. Parses log format, creates a sequence of parsers, parses a single line according to the log format. Now, it has a mapping `magic word` -> `log element`

1. Prepares a mapping `magic word` -> `parser for a log element`

1. Combines the mappings `column name` -> `magic word`, `magic word` -> `log element`, `magic word` -> `parser for a log element` to get a mapping `column name` -> `parsed log element`

1. Prints the result

## TODO

* Provide better error handling
* Write comments