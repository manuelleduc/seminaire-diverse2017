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


## Stats

```
$ cloc app Dockerfile LICENSE README.md seminaire-diverse2017-servant.cabal server.cfg Setup.hs src stack.yaml     
      12 text files.
      12 unique files.                              
       4 files ignored.

github.com/AlDanial/cloc v 1.70  T=0.01 s (630.5 files/s, 17968.2 lines/s)
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Haskell                          6             28              3            167
Markdown                         1              5              0             17
YAML                             1              0              0              8
-------------------------------------------------------------------------------
SUM:                             8             33              3            192
-------------------------------------------------------------------------------
```
