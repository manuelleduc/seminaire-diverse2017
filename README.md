# seminaire-diverse2017-servant

## Installing stack
https://docs.haskellstack.org/en/stable/README/

## Build
```bash
$ stack setup #only the first time
$ stack install
$ stack exec -- seminaire-diverse2017-servant-exe # server running on 8080
```

## Config
server.cfg contains the configurable informations

- **db:** sqlite database
- **port:** http server port 

```
db = "server.db"
port = 8080
```
