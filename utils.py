#!/usr/bin/env python3

import os
import hashlib
import requests
import re


def hash_string(s):
    m = hashlib.sha256()
    m.update(s.encode('utf8'))
    # m.update(b" the spammish repetition")
    return m.hexdigest()

def gen_hash_filename(link):
    # FIXME use system tmp dir
    return 'tmp/' + hash_string(link)

def download_file(url, filename):
    print('downloading ' + url + ' into ' + filename + ' ..')
    if os.path.exists(filename):
        print('exists. Skip')
    else:
        r = requests.get(url)
        with open(filename, 'wb') as f:
            f.write(r.content)
        # print('sleeping 5 sec ..')
        # time.sleep(5)



##############################
## Utility functions
##############################

stop_words = set(["you", "the", "and", "can", "where", "when", "how",
                  "what", "who", "why", "does", "for", "are", "don",
                  "from"])


def title_first_word(title):
    lst = re.split(r'[\s:\-?()/,\'*"#$+“”’]', title.lower())
    lst = list(filter(lambda s: len(s) > 2, lst))
    lst = list(filter(lambda s: s not in stop_words, lst))
    return lst[0].title() if lst else 'Title'


def gen_id(year, conf, authors, title):
    return '-'.join([str(year), str(conf),
                     authors[0].split(' ')[-1], title_first_word(title)])

def authors_str2lst(str):
    return list(map(lambda s: s.strip(), re.split(r',|;|\*| and ', str)))

def clean_string(str):
    return str.replace('\n', ' ').replace('  ', ' ').strip()

def gen_single_bib(id, title, author, pdflink, year, booktitle):
    return ('@inproceedings{' + id + ",\n"
            + "  title={" + clean_string(title) + "},\n"
            + "  author={" + clean_string(author) + "},\n"
            + "  year={" + str(year) + "},\n"
            + "  booktitle={" + booktitle + "},\n"
            + "  pdflink={" + pdflink + "}\n}\n")


def path2year(path):
    return re.search(r'/\w+-(\d{4})', path)[1]

def path2conf(filename):
    return re.search('/(\w+)-\d{4}', filename)[1]
