# Scala 3 dojo

[![Scala](https://img.shields.io/badge/Scala-3-%23DC322F?style=flat&labelColor=%23383838&logo=Scala&logoColor=%23DC322F&logoWidth=12&cacheSeconds=3600)](https://www.scala-lang.org/)

## Setup

Scala-CLI - IntelliJ IDEA setup: [guide](https://scala-cli.virtuslab.org/docs/cookbooks/ide/intellij/).

## Test

Test only a single package with

```bash
scala-cli test --watch . --test-only 'mypackage*'
```

## Run

Run a specific main class with

```bash
scala-cli . --main-class mypackage.Main
```
