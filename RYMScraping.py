import pandas as pd
import numpy as np
import os
import requests
from bs4 import BeautifulSoup
from datetime import datetime as dt

directory = 'C:/Users/trevor.krause/Documents/Projects/RYMGenres/All/'

album_data = pd.DataFrame()

for filename in os.listdir(directory):
    if filename.endswith(".html"):
        print(os.path.join(directory, filename))

        # Select file
        soup = BeautifulSoup(open(os.path.join(directory, filename), encoding='utf-8'), "html.parser")

        # Get Album Info
        artist = soup.find(class_='artist').get_text()
        album = (soup.find(class_='album_title').contents[0]).rstrip()
        score = soup.find(class_='avg_rating').get_text().strip()
        num_ratings = int(soup.find(itemprop='ratingCount').get('content'))
        try:
            release_date = dt.strptime(soup.find(class_='issue_year ymd').get('title').strip(), '%d %B %Y')
        except AttributeError:
            release_date = np.NaN

        temp_df = pd.DataFrame(columns=['artist', 'album', 'release_date', 'score', 'num_ratings'],
                               data=[[artist, album, release_date, score, num_ratings]])

        # Get Genres
        i = 0
        while i < 3:
            try:
                temp_df['genre{}'.format(i+1)] = soup.find(class_='release_pri_genres').\
                    find_all(class_='genre')[i].get_text()
            except IndexError:
                # If there are less than 3 genres
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
            temp_df['desc{}'.format(i+1)] = (soup.find(class_='release_descriptors').find_all('meta')[i]['content'])\
                .strip()

            i += 1

        album_data = album_data.append(temp_df, sort=False)

album_data.to_csv('C:/Users/trevor.krause/Documents/Projects/RYMGenres/fulldata.csv', index=False, encoding='utf-8-sig')
