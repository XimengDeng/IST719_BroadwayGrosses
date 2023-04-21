import csv
import os
import requests
from lxml import etree
from bs4 import BeautifulSoup
from loguru import logger
#Show_name	Show_theatre	This_Week_Gross	Potential_Gross	Difference	Average_ticket	Top_ticket	Seats_Sold	Seats_in_theater	Perfs	Previews	%cap	diff_cap

def open_excel(path):
    if not os.path.exists(path):
        with open(path, 'a', encoding='utf-8-sig', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(('date','Show_name','Show_theatre','This_Week_Gross','Potential_Gross','Difference',
                             'Average_ticket','Top_ticket','Seats_Sold','Seats_in_theater','Perfs','Previews',
                             '%cap','diff_cap'))

def gettime():
    url = 'https://www.playbill.com/grosses?week=2023-01-15'
    header = {
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'}

    html = requests.get(url, headers=header).text
    date=etree.HTML(html).xpath('//*[@id="vault-search-results-sort-select"]/option//text()')
    return date

def get_data(week):
    url='https://www.playbill.com/grosses?week={}'.format(week)
    header={'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36'}
    a=0
    while 1:
        try:
            html=requests.get(url,headers=header,timeout=5)
            code=html.status_code
            if int(code)==200:
                break
        except Exception as e:
            print(e)
            pass
        a=a+1
        if a>=1:
            print('重试:',a)
    html=html.text

    soup = BeautifulSoup(html, 'html.parser')
    nums = len(soup.select("tbody>tr"))
    
    showname = []
    showname_list = soup.find_all("td", class_="col-0")
    for sn in showname_list:
        showname.append(sn.find("span",class_="data-value").text)

    show_theatre = []
    show_theatre_list = soup.find_all("td", class_="col-0")
    for st in show_theatre_list:
        show_theatre.append(st.find("span",class_="subtext").text)

    this_Week_Gross = []
    this_Week_Gross_list = soup.find_all("td", class_="col-1")
    for twgl in this_Week_Gross_list:
        this_Week_Gross.append(twgl.find("span",class_="data-value").text)

    Potential_Gross = []
    Potential_Gross_list = soup.find_all("td", class_="col-1")
    for pgl in Potential_Gross_list:
        Potential_Gross.append(pgl.find("span", class_="subtext").text)

    Difference = []
    Difference_list = soup.find_all("td", class_="col-2")
    for dl in Difference_list:
        Difference.append(dl.find("span",class_="data-value").text)

    average_ticket = []
    average_ticket_list = soup.find_all("td", class_="col-3")
    for atl in average_ticket_list:
        average_ticket.append(atl.find("span",class_="data-value").text)

    top_ticket = []
    top_ticket_list = soup.find_all("td", class_="col-3")
    for ttl in top_ticket_list:
        top_ticket.append(ttl.find("span", class_="subtext").text)

    seats_Sold = []
    seats_Sold_list = soup.find_all("td", class_="col-4")
    for ssl in seats_Sold_list:
        seats_Sold.append(ssl.find("span",class_="data-value").text)

    seats_in_theater = []
    seats_in_theater_list = soup.find_all("td", class_="col-4")
    for sitl in seats_in_theater_list:
        seats_in_theater.append(sitl.find("span", class_="subtext").text)

    perfs = []
    perfs_list = soup.find_all("td", class_="col-5")
    for pl in perfs_list:
        perfs.append(pl.find("span",class_="data-value").text)

    previews = []
    previews_list = soup.find_all("td", class_="col-5")
    for pl in previews_list:
        previews.append(pl.find("span", class_="subtext").text)

    cap = []
    cap_list = soup.find_all("td", class_="col-6")
    for cl in cap_list:
        cap.append(cl.find("span",class_="data-value").text)

    diff_cap = []
    diff_cap_list = soup.find_all("td", class_="col-7")
    for dcl in diff_cap_list:
        diff_cap.append(dcl.find("span",class_="data-value").text)

    
    allinfo=[]
    for x in [showname,show_theatre,this_Week_Gross,Potential_Gross,Difference,average_ticket,top_ticket,\
            seats_Sold,seats_in_theater,perfs,previews,cap,diff_cap]:
        if len(x)==0:
            x=['无' for x in range(nums)]
        allinfo.append(x)
    
    for n in range(nums):
        r=[week]+[x[n] for x in allinfo]
        with open('data.csv','a',encoding='utf-8-sig',newline='')as f:
            writer=csv.writer(f)
            logger.info(r)
            writer.writerow(r)


if __name__ == '__main__':
    path = 'data.csv'
    open_excel(path)
    weeks=gettime()

    for w in weeks:
        if '2017' in w:
            break
        get_data(w)