# repohs

Download repos as zips, unpack them and give SLOC for a specified language

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
* As a script. Assume you put the code into `script.hs`
```sh
chmod +x script.hs
./script.hs
```