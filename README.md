# repohs

Provides a convenient way to download several archives with source code

## How it works
* It defines several constants: 
    * `_PATH` - where all files will be stored
    * `_LANGUAGE` - extension of files written in language of interest. For now, defines a single extension. E.g., `.py` for `Python`
    * `_ZIPPED` - name of subdirectory in `_PATH` where the zipped archives will be stored
    * `_UNZIPPED` - name of subdirectory in `_PATH` where the unzipped folders will be stored
    * `_URLs` - a list of URLs of interest

* It will concurrently download the files and unpack them into corresponding folders
    * Each zip-archive will be named after URL's path: file for `https://github.com/django/django/archive/refs/heads/main.zip` will become `django.django.archive.refs.heads.main.zip`
    * Contents of such archive will be unpacked into subdirectory `django.django.archive.refs.heads.main`

* If you have any questions, have a look at [Main.hs](./app/Main.hs)

## SLOC
* See current results in the [report](./report)

## Requirements
1. `npm`
```sh
sudo apt install nodejs
```
1. A utility for counting code lines ([link](https://github.com/flosse/sloc))
```sh
sudo npm install -g sloc
```
1. `stack` ([link](https://docs.haskellstack.org/en/stable/install_and_upgrade/))

## Running
* From this project's root
```sh
stack run
```
* As a script. Assume you put the code into `script.hs` somewhere
```sh
chmod +x script.hs
./script.hs
```

## Articles
* [Resource Management in Haskell](https://aherrmann.github.io/programming/2016/01/04/resource-management-in-haskell/index.html)