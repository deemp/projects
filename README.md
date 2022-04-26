# Count SLOC in several GH projects

A script for counting SLOC for a `LANGUAGE` in GitHub projects.

## Usage
1. Specify the list of URLs of projects you want to download and process in [pystat.py](./pystat.py)
    * Each URL should lead to either `Tag` or to `main` of a GitHub project.
        * To get a `Tag` link for a project, see `Releases` -> `Tags`, fill the correct `<user>`, `<repo>`, `<tag>` into `https://github.com/<user>/<repo>/archive/refs/tags/<tag>.zip` and use this link
        * To get the freshest zip, fill the correct `<user>` and `<repo>` into `https://github.com/<user>/<repo>/archive/refs/heads/main.zip` and use this link.
    * If the `./repos/unzipped` already contains a directory for a `Tag` or `main`, this `Tag` or `main` won't be downloaded again.
1. Specify the `LANGUAGE` in [pystat.py](./pystat.py)
2. Install the required packages from `requirements.txt` ([link](https://pip.pypa.io/en/stable/cli/pip_freeze/#examples))
1. Install [cloc](https://github.com/AlDanial/cloc) ([link](https://www.geeksforgeeks.org/cloc-count-number-of-lines-of-code-in-file/))
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