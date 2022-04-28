# repohs

Download repos as zips, unpack them into `./repos` and give SLOC for files with a language extension specified in the script (see `_EXTENSION` in [main](./app/Main.hs))

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