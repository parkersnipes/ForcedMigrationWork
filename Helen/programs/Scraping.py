#importing the libraries
from bs4 import BeautifulSoup
import requests
import pandas as pd
import os
import urllib.request
import pandas
import openpyxl

url = "https://en.wikipedia.org/wiki/Municipalities_of_Colombia"
data = requests.get(url)
soup = BeautifulSoup(data.text, 'html.parser')
links = soup.find_all("li")
filtered_links = [x for x in links if ("/wiki/" in str(x) and "Colombia" not in str(x) and "Department" not in str(x))]
filtered_links = filtered_links[:1125]
#print(filtered_links[18])


# There are just 14 bad links left.
# NOTE: 12 excluded since they contain "Colombia"

q = 0
columns = ["muni","lat","lon"]
df = pandas.DataFrame(columns = columns)

for url in filtered_links:
    print(q)
    relevant_piece = str(url).split('\"')[1]
    url = "https://en.wikipedia.org"+relevant_piece
    muni = relevant_piece.split("/")[-1]
    print(muni)
    lat = 0
    lon = 0
    try:
        req = requests.get(url)
        soup = BeautifulSoup(req.content, 'html.parser')
        html = str(soup)

        # Somewhere in the HTML, near the beginning, is a scrap of JSON encoding
        # the latitude and longitude of the power plant. Find it and parse it.
        try:
            idx = html.index('"wgCoordinates"')
            i, j = html[idx:].index('{') + idx, html[idx:].index('}') + idx
            s_js = html[i:j+1]
            pieces = s_js.split(",")
            lat = pieces[0].split(":")[1]
            lon = pieces[1].split(":")[1][:-1]
            print(lat,lon)
        except:
            lat = float("nan")
            lon = float("nan")
    except:
        lat = float("nan")
        lon = float("nan")

    df.loc[len(df.index)] = [muni,lat,lon]
    q+=1
print(df)
writer = pd.ExcelWriter("MuniLatLon.xlsx",options={'strings_to_numbers': True})
df.to_excel(writer)
writer.save()
