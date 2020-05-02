# scraper-atcoder

## usage

First, run the scraper at a task page in the contest.

```shell
$ scraper-atcoder <url>
```

Next, you get input/expect pairs with some noises.

And then, you remove noisy files **manually**.

Finally, you can golden-test with these files.

## dependencies

- selenium-server (2.x)
  - `wget https://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.1.jar`
  - and require jre 8.
- chromedriver
  - `wget https://chromedriver.storage.googleapis.com/83.0.4103.14/chromedriver_linux64.zip`
  - and then unzip it.
