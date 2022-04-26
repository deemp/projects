# Count SLOC in several GH projects

A script for counting `{target_language}` SLOC in GitHub projects.

1. Specify the list of URLs of projects you want to download and process in [pystat.py](./pystat.py)
    * Each URL should lead to a `Tag` of a GitHub project. For a project, see `Releases` -> `Tags`.
    * If the `./repos/unzipped` directory already contains a directory for a `Tag`, this `Tag` won't be downloaded again.
2. Install the required packages from `requirements.txt` ([link](https://pip.pypa.io/en/stable/cli/pip_freeze/#examples))
3. Run the script
```sh
python3 pystat.py
```

See current statistics [here](./current_stat)