#!/usr/bin/env python3

# IEEE paper download

# https://ieeexploreapi.ieee.org/api/v1/search/articles?parameter&apikey=
# https://ieeexploreapi.ieee.org/api/v1/search/articles?publication_year=2018

from bs4 import BeautifulSoup
import os
import requests
from bibgen import gen_id, gen_single_bib
import re
import time

import hashlib

def hash_string(s):
    m = hashlib.sha256()
    m.update(s.encode('utf8'))
    # m.update(b" the spammish repetition")
    return m.hexdigest()


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
        
def ieee_index_page(pnumber):
    """Return a list [title] and [link] for each conference year. The
    year,link pair should be determined outside.

    """
    # if not exist, download to a tmp file
    # https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000639
    # parse the file
    # get the list of pnumber for each year, and return
    url = 'https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=' + str(pnumber)
    html_file = gen_hash_filename(url)
    if not os.path.exists(html_file):
        download_file(url, html_file)
    soup = BeautifulSoup(open(html_file))
    texts = []
    links = []
    for a in soup.select('.detail li a'):
        text = a.get_text().strip()
        link = a['href']
        if not link.startswith('http'):
            link = 'https://ieeexplore.ieee.org/xpl/' + link
        texts.append(text)
        links.append(link)
    return list(reversed(texts)), list(reversed(links))

def ieee_journal_index_page(punumber):
    """Return a {year: [links]}
    """
    # https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=8860
    # punumber=8860
    url = 'https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=' + str(punumber)
    html_file = gen_hash_filename(url)
    if not os.path.exists(html_file):
        download_file(url, html_file)
    soup = BeautifulSoup(open(html_file))
    volumes = {}
    for ul in soup.select('.volumes ul'):
        # pi-2012
        year = int(ul['id'].split('-')[1])
        if not year in volumes:
            volumes[year] = []
        for a in ul.select('li a'):
            link = a['href']
            if not link.startswith('http'):
                link = 'https://ieeexplore.ieee.org/' + link
            volumes[year].append(link)
    return volumes
    
    

def gen_hash_filename(link):
    # FIXME use system tmp dir
    return 'tmp/' + hash_string(link)

def ieee_conference_get_pagination(year, conf, link):
    """Return [link] for all pagination of this conference
    """
    html_file = gen_hash_filename(link)
    if not os.path.exists(html_file):
        download_file(link, html_file)
    soup = BeautifulSoup(open(html_file))
    res = []
    # the first page
    res.append(link)
    # if have pagination
    if soup.select('.pagination a'):
        pagenum = int(soup.select('.pagination a')[-2].get_text().strip())
        hidden = soup.select('input#oqs')[0]['value']
        # the first is obit
        for page in range(1, pagenum):
            # https://ieeexplore.ieee.org/xpl/mostRecentIssue.jsp?punumber=8449910&filter=issueId%20EQ%20%228460178%22&pageNumber=2
            link += '&' + hidden + '&pageNumber=' + str(page+1)
            res.append(link)
    return res

def ieee_conference_bib_with_pagination(year, conf, link):
    res = ''
    pages = ieee_conference_get_pagination(year, conf, link)
    print('total pages to process: ', len(pages))
    for page in pages:
        res += ieee_conference_bib(year, conf, page)
    return res
    
def ieee_conference_bib(year, conf, link):
    html_file = gen_hash_filename(link)
    if not os.path.exists(html_file):
        download_file(link, html_file)
    soup = BeautifulSoup(open(html_file))
    res = ''
    for div in soup.select('.txt'):
        if div.h3.a and div.select('.authors a'):
            title = div.h3.get_text()
            # "/document/8461262/"
            arnumber = re.findall(r'/(\d+)/', div.h3.a['href'])[0]
            pdflink = 'https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=' + arnumber
            authors = [author.get_text().strip() for author in div.select('.authors a')]
            # print(authors)
            id = gen_id(year, conf, authors, title)
            bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
            res += bib
    return res

def icra_bib(year):
    icra_index_pnumber = '1000639'
    _, links = ieee_index_page(icra_index_pnumber)
    years = list(range(1984, 2019))
    link = links[year - 1984]
    return ieee_conference_bib_with_pagination(year, 'ICRA', link)

def tro_bib(year):
    tro_index_punumber = 8860
    # TODO TRO has two more titles. This one only cover [2004,]
    volumes = ieee_journal_index_page(tro_index_punumber)
    res = ''
    if year in volumes:
        links = volumes[year]
        for link in links:
            res += ieee_conference_bib_with_pagination(year, 'TRO', link)
            # sleep for journal
            print('sleeping 3 sec ..')
            time.sleep(3)
    return res

def gen_bib(conf, bib_func, year):
    bib_dir = '/home/hebi/github/smart-scholar-dist/bib/' + conf
    if not os.path.exists(bib_dir):
        os.makedirs(bib_dir)
    bib_file = os.path.join(bib_dir, conf + '-' + str(year) + '.bib')
    if not os.path.exists(bib_file):
        bib = bib_func(year)
        with open(bib_file, 'w') as f:
            f.write(bib)

        
def gen_tro_all():
    for year in range(2004, 2019):
        print('----', year)
        gen_tro(year)
        print('sleeping 10 sec ..')
        time.sleep(10)
        
def gen_tro(year):
    gen_bib('TRO', tro_bib, year)

def gen_icra(year):
    gen_bib('ICRA', icra_bib, year)
    
def iros_bib(year):
    iros_index_pnumber = '1000393'
    _, links = ieee_index_page(iros_index_pnumber)
    years = list(range(1988, 2018))
    link = links[year - 1988]
    return ieee_conference_bib_with_pagination(year, 'IROS', link)

def gen_iros(year):
    gen_bib('IROS', iros_bib, year)

def gen_icra_all():
    for year in range(1984, 2019):
        print('----', year)
        gen_icra(year)
        print('sleeping 10 sec ..')
        time.sleep(10)

def gen_iros_all():
    for year in range(1988, 2018):
        print('----', year)
        gen_iros(year)
        print('sleeping 10 sec ..')
        time.sleep(10)
    

if __name__ == '__hebi__':
    # download conference index
    # extract conference pnumber by year
    # download conference html by year
    # parse for bib
    # ICRA: https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000639
    # https://ieeexplore.ieee.org/servlet/opac?punumber=1000639
    ieee_index_page('1000639')
    bib = icra_bib(2018)
    gen_icra(2018)
    gen_iros(2017)
    gen_icra_all()
    gen_iros_all()
    bib = tro_bib(2018)
    gen_tro_all()
    # gen_id(1028, 'hfh', 'he', 'a new')
    pass
