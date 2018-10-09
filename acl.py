#!/usr/bin/env python3

# https://aclanthology.info/


# https://aclanthology.info/events/acl-1997
def acl_bib(year):
    link = 'https://aclanthology.info/events/acl-' + str(year)
    return acl_conference_bib(year, 'ACL', link)

def acl_conference_bib(year, conf, link):
    html_file = gen_hash_filename(link)
    if not os.path.exists(html_file):
        download_file(link, html_file)
    soup = BeautifulSoup(open(html_file))
    res = ''
    for p in soup.select('.content p'):
        
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

