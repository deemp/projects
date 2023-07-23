#%%
from concurrent.futures import ThreadPoolExecutor
import json
import os
from os import makedirs
from os.path import join
from urllib.request import urlopen
from concurrent.futures import ThreadPoolExecutor
import json
import zipfile
import glob
#%%


# list of tags from releases
# See Releases -> Tags on github
URLS = [
    "https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.5.1.zip",
    "https://github.com/django/django/archive/refs/tags/4.0.4.zip",
]

# local folder where zips and unpacked code will be stored
PATH = 'repos'

# target metric
# see https://github.com/AlDanial/cloc#options-
CODE_JSON = "code"

# target language
# 
LANGUAGE = "Python"

#%%

def get_name_from_url(url):
    repo_name = url.split("/")[-5]
    tag = os.path.basename(url)
    return f"{repo_name}-{tag}"

def zipped(x): return f"{x}/zipped"
def unzipped(x): return f"{x}/unzipped"

def get_existing_projects():
    return [os.path.basename(i) for i in glob.glob(f"./{unzipped(PATH)}/*")]

def strip_zip(s): return s.replace(".zip", "")

existing = get_existing_projects()
URLS = [i for i in URLS if strip_zip(get_name_from_url(i)) not in existing] 

print(URLS)

#%%
# download a url and return the raw data, or None on error
def download_url(url):
    try:
        # open a connection to the server
        with urlopen(url, timeout=3) as connection:
            # read the contents of the html doc
            return connection.read()
    except:
        # bad url, socket timeout, http forbidden, etc.
        return None
 
# save data to a local file
def save_file(url, data, path):
    # get the name of the file from the url
    file_name = get_name_from_url(url)
    # construct a local path for saving the file
    out_path = join(path, file_name)
    # save to file
    with open(out_path, 'wb') as file:
        file.write(data)
    return out_path
 

def unzip_file(file_path, extract_to):
    # https://stackoverflow.com/a/3451150
    with zipfile.ZipFile(file_path, 'r') as zip_ref:
        new_path = f"{extract_to}/{strip_zip(os.path.basename(file_path))}"
        zip_ref.extractall(new_path)

# download and unzip an archive
def download_and_process(url, path):
    
    # download the url
    print(f'Downloading {url}...')
    data = download_url(url)
    # check for no data
    if data is None:
        print(f'>Error downloading {url}')
        return
    # save the data to a local file

    out_path = save_file(url, data, zipped(path))
    
    # report progress
    print(f'>Saved {url} to {out_path}')

    unzipped_path = unzipped(path)
    unzip_file(file_path=out_path, extract_to=unzipped_path)
    unzipped_name = strip_zip(os.path.basename(out_path))
    print(f'>Unzipped {out_path} into {unzipped_path}/{unzipped_name}')


# download a list of URLs to local files
def download_repos(urls, path):
    # create the local directory, if needed
    makedirs(zipped(path), exist_ok=True)
    makedirs(unzipped(path), exist_ok=True)
    # create the thread pool
    n_threads = len(urls) + 1
    with ThreadPoolExecutor(n_threads) as executor:
        # download each url and save as a local file
        _ = [executor.submit(download_and_process, url, path) for url in urls]



#%%
# download all docs
download_repos(URLS, PATH)

print("Finished unzipping")

#%%

#%%
# https://stackoverflow.com/a/71512847
import sys

class DuplicateStdout:
    def __init__(self, path):
        self.stdout = sys.stdout
        self.path = path
        self.f = None
    
    def write(self, s):
        self.stdout.write(s)
        self.f.write(s)

    def __enter__(self):
        self.f = open(self.path, "w")
        sys.stdout = self
    
    def __exit__(self, *args):
        sys.stdout = self.stdout
        self.f.close()


# Collect statistics about repositories
def count_lines(language):
    stream = os.popen(f'cloc --json {unzipped(PATH)}')
    output = stream.read().strip()
    j = json.loads(output)[language][CODE_JSON]
    return j


def get_description(language=LANGUAGE):
    sloc = count_lines(language=language)
    projects = get_existing_projects()
    print(f"""Project{"s" if len(projects) > 1 else ""}:\n""")
    for i in projects: print(i)
    print(f"""\ncontain{"s" if len(projects) == 1 else ""} {sloc} {language} SLOC""")

print("Counting lines...")

with DuplicateStdout('current_info'):
    get_description(language=LANGUAGE)
# %%
