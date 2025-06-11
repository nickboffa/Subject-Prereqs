import requests
from bs4 import BeautifulSoup
import json

with open("/data/current/course_codes.txt", "r") as f:
    codes = [line.strip() for line in f if line.strip()]

blurbs = {}
names = {}
for code in codes:
    url = f'https://programsandcourses.anu.edu.au/course/{code}'
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')

    #GETTING THE NAME OF THE COURSE
    name = soup.find('title').text
    name = name[:-6] #removing the hidden ' - ANU' in every name

    #GETTING 'REQUISITES AND INCOMPATIBILITY' SECTION
    try:
        requisite_text = soup.find('div', class_='requisite').text
    except AttributeError: #if no requisite section
        requisite_text = ''

    requisite_text = requisite_text.replace('\n', '')
    requisite_text = ' '.join(requisite_text.split())

    #ADDING TO DICTIONARIES
    names[code] =  name 
    blurbs[code] = requisite_text

    print(code, name)


#WRITE BLURBS AND NAMES TO TEXT FILE
with open('names.txt', 'w') as convert_file:
    convert_file.write(json.dumps(names))

with open('blurbs.txt', 'w') as convert_file:
    convert_file.write(json.dumps(blurbs))