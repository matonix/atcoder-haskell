# scraper-atcoder

## usage

First, run selenium-server.

```shell
$ java -jar selenium-server-standalone-2.53.1.jar
```

And then, run screper.

```shell
$ scraper-atcoder -c CONTEST_ID -u USERNAME -p PASSWORD
```

## dependencies

- selenium-server (2.x)
  - `wget https://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.1.jar`
- jre 8