# Script to scrape author data, as this data is in the byline and not scraped by the other code
# Uses ao3 API
# Seow 2020

# dependacies
from ao3 import AO3
import pandas as pd
import os

# load meta_data to get fanwork ids
path = 'C:\Users\dream\Documents\GitHub\NiFAO3Scrape'
os.chdir(path)

metaData = pd.read_csv(r'nif_metaData.csv')
print(metaData.work_id)

# start ao3 api
api = AO3()


# empty df
df = []
errDf = []


# loop to get ao3 data
 # Known issues with current ao3 api (https://github.com/alexwlchan/ao3):
# 1) if >1 author, api may throw an assertion error
# - if commenting he assertion line out in works.py, this results in scraping only the 1st author
# - modifying it to scrape >1 data will result in content that cannot be encoded (i.e. will have the 'u') 

for work_id in range(len(metaData.work_id)):
   print(work_id)
   print(metaData.work_id[work_id])
   try:
       fanwork = api.work(id=metaData.work_id[work_id])
       df.append([fanwork.id,  fanwork.author.encode("utf-8"), fanwork.title.encode("utf-8")])
   except Exception as e:
      print(e)
      errDf.append(work_id)
      pass

authorDf = pd.DataFrame(df, columns=('work_id', 'author', 'title_check'))
authorDf.to_csv('nif_metaData_author.csv', index = False)

