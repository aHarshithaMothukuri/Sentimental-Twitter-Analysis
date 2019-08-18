# -*- coding: utf-8 -*-
"""Sentiment_Analysis.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1H1IXg87hhEyGnSO2EQLPosVoNmPbtbwQ
"""

!pip install tweepy

import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tweepy
from datetime import datetime

consumer_secret = "m2mRm7xH8ti2v5Uax81RZSKq07qFLx4X93SaHb3nNWebvMXdsz"
consumer_key = "aeZnM0UCukSwaPy8RNKcsMMvh"

access_token = "1661967871-PFSQD0Hatf3YzFes7O3dIJ8mNuXwZnIfsNNVv10"
access_token_secret = "n0DKLo7N6nPr4AB00x7lHx JtVjZfBxCrZiwjBTRP9u5TF"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth,parser=tweepy.parsers.JSONParser())

! pip install vaderSentiment
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer =SentimentIntensityAnalyzer()

target_users=["@republic","@ndtv","@TimesNow","@IndiaToday","@CNNnews18"]
sentiments = []

for target in target_users:
  print(target)
  counter= 0
  
  compound_list = []
  positive_list = []
  negative_list = []
  neutral_list  = [] 
  
  
  public_tweets = api.user_timeline(target, count=200)
  
  # Loop through each tweet
  
  for tweet in public_tweets:
    
    results = analyzer.polarity_scores(tweet["text"])
    compound =results["compound"]
    pos = results["pos"]
    neu = results["neu"]
    neg = results["neg"]
    tweets_ago = counter
    
    sentiments.append({"Tweet":tweet["text"],"News Org":target,"Tweets Ago":counter,"Date":tweet["created_at"],"Compound":compound,"Positive":pos,"Negative":neg,"Neutral":neu})
    counter = counter+1

sentiments_pd = pd.DataFrame.from_dict(sentiments)
sentiments_pd = sentiments_pd[['News Org','Tweets Ago','Date','Tweet','Compound','Positive','Negative','Neutral']]

df=sentiments_pd.to_csv("new.csv",encoding="utf-8",index = False)

orgs_colors_dict = {'@republic':'lightblue','@ndtv': 'green','@TimesNow': 'red','@IndiaToday': 'blue','@CNNnews18': 'gold'}

plt.scatter(sentiments_pd.groupby(["News Org"]).get_group("@republic")["Tweets Ago"],
                sentiments_pd.groupby(["News Org"]).get_group("@republic")["Compound"],
                  facecolors=orgs_colors_dict['@republic'], edgecolors='black', label="Republic Bharat")
plt.scatter(sentiments_pd.groupby(["News Org"]).get_group("@ndtv")["Tweets Ago"],
                sentiments_pd.groupby(["News Org"]).get_group("@ndtv")["Compound"],
                  facecolors=orgs_colors_dict['@ndtv'], edgecolors='black', label="NDTV")
plt.scatter(sentiments_pd.groupby(["News Org"]).get_group("@TimesNow")["Tweets Ago"],
                sentiments_pd.groupby(["News Org"]).get_group("@TimesNow")["Compound"],
                  facecolors=orgs_colors_dict['@TimesNow'], edgecolors='black', label="Times Now")
plt.scatter(sentiments_pd.groupby(["News Org"]).get_group("@IndiaToday")["Tweets Ago"],
                sentiments_pd.groupby(["News Org"]).get_group("@IndiaToday")["Compound"],
                  facecolors=orgs_colors_dict['@IndiaToday'], edgecolors='black', label="India Today")
plt.scatter(sentiments_pd.groupby(["News Org"]).get_group("@CNNnews18")["Tweets Ago"],
                sentiments_pd.groupby(["News Org"]).get_group("@CNNnews18")["Compound"],
                  facecolors=orgs_colors_dict['@CNNnews18'], edgecolors='black', label="CNN News18")

now = datetime.now()
now = now.strftime("%m/%d/%y")
plt.title(f'Sentiment Analysis of Media Tweets ({now})')
plt.xlabel("Tweets Ago")
plt.ylabel("Tweet Polarity")

plt.xlim(100, 0)
plt.ylim(-1.0, 1.0)
yticks = [-1.0, -0.5, 0.0, 0.5, 1.0]
plt.yticks(yticks)

plt.legend(title="Media Sources", bbox_to_anchor=(1, 1), frameon=False)

plt.savefig("sentiment_analysis_of_media_tweets.png")
plt.show()

x_axis = np.arange(sentiments_pd['News Org'].nunique())
tick_location = [value+0.4 for value in x_axis]

plt.title(f'overall twitter sentiment based on twitter ({now})')
plt.xlabel("Media Source")
plt.ylabel("Tweet Polarity")

plt.bar(x_axis, sentiments_pd.groupby("News Org").mean()["Compound"],
       color = orgs_colors_dict.values(),align="edge",width =1)
plt.xticks(tick_location,sentiments_pd["News Org"].unique())

plt.savefig("bar_sentiment.png")
plt.show()

! pip install wordcloud

# Commented out IPython magic to ensure Python compatibility.
import matplotlib as mpl
import matplotlib.pyplot as plt
# %matplotlib inline

from subprocess import check_output
from wordcloud import WordCloud, STOPWORDS

#mpl.rcParams['figure.figsize']=(8.0,6.0)    #(6.0,4.0)
mpl.rcParams['font.size']=12                #10 
mpl.rcParams['savefig.dpi']=100             #72 
mpl.rcParams['figure.subplot.bottom']=.1 


stopwords = set(STOPWORDS)
data = pd.read_csv("new.csv")

wordcloud = WordCloud(
                          background_color='white',
                          stopwords=stopwords,
                          max_words=200,
                          max_font_size=40, 
                          random_state=42
                         ).generate(str(data['Tweet']))

import matplotlib as mpl

print(wordcloud)
fig = plt.figure(1)
plt.imshow(wordcloud)
plt.axis('off')
plt.show()
fig.savefig("word1.png", dpi=900)

