#!/usr/bin/env python3

from ieee import tro_bib, icra_bib, iros_bib
from acl import acl_bib, cl_bib, naacl_bib, eacl_bib, emnlp_bib
import time

def gen_bib(conf, bib_func, year):
    """bib_func should be (lambda (year))
    """
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

def gen_acl(year):
    gen_bib('ACL', acl_bib, year)
    
def gen_cl(year):
    gen_bib('CL', cl_bib, year)
def gen_eacl(year):
    gen_bib('EACL', eacl_bib, year)
def gen_naacl(year):
    gen_bib('NAACL', naacl_bib, year)
def gen_emnlp(year):
    gen_bib('EMNLP', emnlp_bib, year)

def gen_acl_all():
    # TODO before 2000
    for year in range(2000, 2019):
        print('----', year)
        gen_acl(year)
        print('sleeping 4 sec ..')
        time.sleep(4)
def gen_cl_all():
    # TODO before 2000
    for year in range(2000, 2019):
        print('----', year)
        gen_cl(year)
        print('sleeping 4 sec ..')
        time.sleep(4)
def gen_naacl_all():
    # 18 16 15 13 12 10 09 07 06 04 03 01 00
    # ALL
    for year in [2000, 2001, 2003, 2004, 2006, 2007, 2009, 2010, 2012,
                 2013, 2015, 2016, 2018]:
        print('----', year)
        gen_naacl(year)
        print('sleeping 4 sec ..')
        time.sleep(4)
def gen_eacl_all():
    # 17 14 12 09 06 03
    # TODO before 2000
    for year in [2003, 2006, 2009, 2012, 2014, 2017]:
        print('----', year)
        gen_eacl(year)
        print('sleeping 4 sec ..')
        time.sleep(4)
def gen_emnlp_all():
    # ALL
    for year in range(1996, 2018):
        print('----', year)
        gen_emnlp(year)
        print('sleeping 4 sec ..')
        time.sleep(4)

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
    gen_acl(2000)
    gen_acl_all()
    gen_cl_all()
    gen_naacl_all()
    gen_eacl_all()
    gen_emnlp_all()
    # gen_id(1028, 'hfh', 'he', 'a new')
    pass
