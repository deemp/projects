# Count SLOC in several GH projects

A script for counting SLOC for a `LANGUAGE` in GitHub projects.

## Usage
1. Specify the list of URLs of projects you want to download and process in [pystat.py](./pystat.py)
    * Each URL should lead to a `Tag` of a GitHub project. For a project, see `Releases` -> `Tags`.
    * If the `./repos/unzipped` already contains a directory for a `Tag`, this `Tag` won't be downloaded again.
1. Specify the `LANGUAGE` in [pystat.py](./pystat.py)
2. Install the required packages from `requirements.txt` ([link](https://pip.pypa.io/en/stable/cli/pip_freeze/#examples))
3. Run the script
```sh
python3 pystat.py
```
4. You may remove `repos/zipped` now
```sh
rm -rf ./repos/zipped
```
## Info
See current info [here](./current_stat)