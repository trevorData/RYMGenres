import pandas as pd
import numpy as np
import os
import requests
from bs4 import BeautifulSoup
from datetime import datetime as dt


directory = 'C:/Users/trevor.krause/Documents/Projects/RYMGenres/'

album_data = pd.DataFrame()

for filename in os.listdir(directory):
    if filename.endswith(".html"):
        print(os.path.join(directory, filename))

        # Select file
        soup = BeautifulSoup(open(os.path.join(directory, filename), encoding='utf-8'), "html.parser")

        # Get Album Info
        temp_df = pd.DataFrame(columns=['artist', 'album', 'release_date', 'score'],
                               data=[[soup.find(class_='artist').get_text(),
                                      (soup.find(class_='album_title').contents[0]).rstrip(),
                                      dt.strptime(soup.find(class_='issue_year ymd').get('title').strip(), '%d %B %Y'),
                                      soup.find(class_='avg_rating').get_text().strip()
                                      ]])

        # Get Genres
        i = 0
        while i < 2:
            try:
                temp_df['genre{}'.format(i+1)] = soup.find(class_='release_pri_genres').\
                    find_all(class_='genre')[i].get_text()
            except IndexError:
                # If there are less than 2 genres
                temp_df['genre{}'.format(i+1)] = np.nan

            i += 1

        # Get Secondary Genres
        i = 0
        while i < 4:
            try:
                temp_df['sec_genre{}'.format(i+1)] = soup.find(class_='release_sec_genres').\
                    find_all(class_='genre')[i].get_text()
            except (IndexError, AttributeError):
                # If there are less than 4 secondary genres
                temp_df['sec_genre{}'.format(i+1)] = np.nan

            i += 1

        # Get Descriptors
        i = 0
        for desc in soup.find(class_='release_descriptors').find_all('meta'):
            temp_df['desc{}'.format(i+1)] = soup.find(class_='release_descriptors').find_all('meta')[i]['content']

            i += 1

        album_data = album_data.append(temp_df, sort=False)

album_data.to_csv('C:/Users/trevor.krause/Documents/Projects/RYMGenres/dftest.csv', index=False)
