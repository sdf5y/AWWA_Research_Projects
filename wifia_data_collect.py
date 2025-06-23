import numpy as np
import pandas as pd
!pip install PyPDF2 openpyxl
import PyPDF2
import re
import os
!pip install xlsxwriter
import xlsxwriter
#!pip install geopy
#!pip install Nominatim

import plotly.express as px
from plotly.offline import plot
#!pip install folium
import folium

def textscrape (pdf_file_in):
  pdf_file = open(pdf_file_in, 'rb')
  reader = PyPDF2.PdfReader(pdf_file)
  text = ''
  for page_num in range(len(reader.pages)):
    text += reader.pages[page_num].extract_text()

  def remove(string):
    return string.replace(" ", "").replace("\n", "")

  text = text.replace(" ", "").replace("\n", ",")
  pdf_file.close()
  return text

#number search, not $ though
def search_num (pdf_file, pattern1):
  def remove(string):
   return string.replace(" ", "").replace("\n", "")

  text = textscrape(pdf_file)
  pattern1 = remove(pattern1)

  pattern = pattern1 + r'\s*([\d,]+(?:\.\d+)?(?:\s*(?:million|billion))?)?'   #r'\s*([\d,]+(?:\.\d*0)?)'
  matches = re.findall(pattern, text, re.IGNORECASE)

  return(matches)

#name search
def search_name (pdf_file, pattern1):
  def remove(string):
   return string.replace(" ", "").replace("\n", "")

  text = textscrape(pdf_file)
  pattern1 = remove(pattern1)

  pattern = pattern1 + r'\s*([^,]*,[^,]*)'      #r'\s*([^,]+[,])'
  matches = re.findall(pattern, text, re.IGNORECASE)

  return(matches)

#number search for $ only
def search_num_dol (pdf_file1, pattern1):
  def remove(string):
   return string.replace(" ", "")

  text = textscrape(pdf_file1)
  pattern1 = remove(pattern1)

  pattern = pattern1 + r'\s*(\$[\d,]+(?:\.\d+)?(?:\s*(?:million|billion))?)?'             #r'\s*\$([\d,]+(?:\.\d+)?\s*(?:million|billion))'
  matches = re.findall(pattern, text, re.IGNORECASE)

  return(matches)

def converttonum (x):
  ins = []
  for value in x:
    if 'million'in value:
        numeric_part = float(re.search(r'([\d,]+(?:\.\d+)?)\s*million', value).group(1))
        ints.append(numeric_part * 1000000)
    elif 'billion' in value:
        numeric_part = float(re.search(r'([\d,]+(?:\.\d+)?)\s*billion', value).group(1))
        ints.append(numeric_part * 1000000000)
    elif "NA" in value:
        ints.append("NA")
    else:
        ints.append(value)
  return ints

def test(pdfname1):
    keywords = ['BORROWER:', 'LOCATION:', 'WIFIA LOAN AMOUNT[s]?:',
                'TOTAL WIFIA PROJECT COSTS:', 'POPULATION SERVED BY \s*(?:system|project[s]?)? :', 'NUMBER OF JOBS CREATED:']

    data = []

    for keyword in keywords:
      if keyword in ['BORROWER:', 'LOCATION:']:
        values = search_name(pdfname1, keyword)
      elif keyword in ['WIFIA LOAN AMOUNT[s]?:', 'TOTAL WIFIA PROJECT COSTS:']:
        values = search_num_dol(pdfname1, keyword)
      elif keyword in ['POPULATION SERVED BY \s*(?:system|project[s]?)? :', 'NUMBER OF JOBS CREATED:']:
        values = search_num(pdfname1, keyword)
      else:
        values = []

      for value in values:
          if value != "NoneType":
                data.append({'Keyword': keyword, pdfname1: value})
          if value == "NoneType":
                  break
    return pd.DataFrame(data)

file_names = os.listdir()
#file_names.remove('.ipynb_checkpoints')
file_names = file_names[1:(len(file_names)-1)]

keywords = ['BORROWER:', 'LOCATION:', 'WIFIA LOAN AMOUNT[s]?:',
            'TOTAL WIFIA PROJECT COSTS:', 'POPULATION SERVED BY \s*(?:system|project[s]?)? :', 'NUMBER OF JOBS CREATED:']
words = ["wastewater", "drinking water", "stormwater", "reuse"]

master_df = pd.DataFrame(keywords)
master_df.columns = ["Keyword"]

for i in range(0, len(file_names)):
    df = test(file_names[i])
    master_df = master_df.merge(df, on= "Keyword", how='left')

master_dfT = master_df.set_index('Keyword').T

temp_df = []
for file_name in file_names:
    found_keywords = []

    for keyword in words:
        if search_name(file_name, keyword):
            found_keywords.append(keyword)

    temp_df.append((found_keywords, file_name))

temp_df = pd.DataFrame(temp_df)
temp_df.columns = ["Project Type", "Keyword"]
master_dfT = master_dfT.merge(temp_df, on='Keyword')

master_dfT.iloc['Keyword']

master_dfT.to_csv('12data.csv', index=False)

data = pd.read_excel('Clean_WIFIA - Copy.xlsx')

coordinate_df = []
loc_list = data['LOCATION:']
from geopy.geocoders import Nominatim

for i in range(0, len(loc_list)):
  cell = loc_list[i]
  address= cell
  geolocator = Nominatim(user_agent="Your_Name")
  location = geolocator.geocode(address)
  print(location.address)
  print((location.latitude, location.longitude))
  coordinate_df.append([location.address, location.latitude, location.longitude])

data = pd.read_excel('Final_Clean.xlsx')

jitter = .09

data["LAT_jittered"] = data["LAT"] + (2 * (pd.np.random.rand(len(data)) - 0.5) * jitter)
data["LONG_jittered"] = data["LONG"] + (2 * (pd.np.random.rand(len(data)) - 0.5) * jitter)

data.columns

#WIFIA Loan Amount Map
custom_colors = {
    'Stormwater':'lime',
    'Drinking water':'cyan',
    'Wastewater':'red',
    'Reuse': 'purple',
}

fig = px.scatter_mapbox(data, lat="LAT_jittered", lon="LONG_jittered",
                        color="PROJECT TYPE:",
                        size='WIFIA LOAN AMOUNT:',                  #'POPULATION SERVED BY PROJECT :' , 'NUMBER OF JOBS CREATED:', 'WIFIA LOAN AMOUNT:', 'TOTAL WIFIA PROJECT COSTS:', 'Loan Percapita'
                        size_max = 30,
                        zoom=3,
                        color_discrete_map = custom_colors,
                        mapbox_style='carto-positron',
                        opacity=.7)

fig.update_layout(legend=dict(
    title="Project Type",
    traceorder="reversed",
    itemsizing="constant",
    itemclick="toggleothers"
))

plot(fig, auto_open=True)

#Population Map
custom_colors = {
    'Stormwater':'lime',
    'Drinking water':'cyan',
    'Wastewater':'red',
    'Reuse': 'purple',
}

fig = px.scatter_mapbox(data, lat="LAT_jittered", lon="LONG_jittered",
                        color="PROJECT TYPE:",
                        size='POPULATION SERVED BY PROJECT :', #'NUMBER OF JOBS CREATED:', 'WIFIA LOAN AMOUNT:', 'TOTAL WIFIA PROJECT COSTS:', 'Loan Percapita'
                        size_max = 30,
                        zoom=3,
                        color_discrete_map = custom_colors,
                        mapbox_style='carto-positron',
                        opacity=.7)

fig.update_layout(legend=dict(
    title="Project Type",
    traceorder="reversed",
    itemsizing="constant",
    itemclick="toggleothers"
))

plot(fig, auto_open=True)

#Jobs Map
custom_colors = {
    'Stormwater':'lime',
    'Drinking water':'cyan',
    'Wastewater':'red',
    'Reuse': 'purple',
}

fig = px.scatter_mapbox(data, lat="LAT_jittered", lon="LONG_jittered",
                        color="PROJECT TYPE:",
                        size='NUMBER OF JOBS CREATED:', #'WIFIA LOAN AMOUNT:', 'TOTAL WIFIA PROJECT COSTS:', 'Loan Percapita'
                        size_max = 30,
                        zoom=3,
                        color_discrete_map = custom_colors,
                        mapbox_style='carto-positron',
                        opacity=.7)

fig.update_layout(legend=dict(
    title="Project Type",
    traceorder="reversed",
    itemsizing="constant",
    itemclick="toggleothers"
))

plot(fig, auto_open=True)

#Loans Per Capita Map
custom_colors = {
    'Stormwater':'lime',
    'Drinking water':'cyan',
    'Wastewater':'red',
    'Reuse': 'purple',
}


fig = px.scatter_mapbox(data, lat="LAT_jittered", lon="LONG_jittered",
                        color="Loan Percapita",
                        color_continuous_scale="RdBu",
                        size_max=30,
                        zoom=3,
                        color_discrete_map=custom_colors,
                        mapbox_style='carto-positron',
                        opacity=0.7)

# Add a color bar legend for the size
fig.update_layout(coloraxis_colorbar=dict(title="Loan Percapita"))

plot(fig, auto_open=True)
