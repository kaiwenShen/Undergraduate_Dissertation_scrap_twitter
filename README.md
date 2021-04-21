 # Dissertation 
This is a repo for my undergrade dissertation, the twitter scrape part is from https://github.com/israel-dryer/Twitter-Scraper/blob/main/twitter_scraper.py with some adjustment to fit my poor internet connection, and the delta tf-idf is based on https://kgptalkie.com/amazon-and-imdb-review-sentiment-classification-using-spacy/. 

In order to run the code, in addition to download the code and have a working python environment with all the packages required, you need to have a selenium webdriver properly setup in your computer. In order to do that you can follow this link: https://selenium-python.readthedocs.io/installation.html, or video https://www.youtube.com/watch?v=9XAH_TvxwLg&t=29s
you also need a working twitter account & password, as suggested in the python script. 

Also,the twitter folder already have the scraped raw tweets, and the df_res folder already have the sentiment analysis result processed, the twitter search key words are "Comscore", "Rentrak", "Comscore Merger", "Rentrak Merger". 

# Short-term Event Study Methodology

The comscore.csv and rentrak.csv are the data collected from WRDS CRSP 

make sure the R code and these file are under the same root when you run the code

in ***st_res folder*** is the dataframe result of the data, only particularly for the event study intermediary process, if examiner wishes to check other computational data such as CAR, Patell Z, p value, then running R is required. In the final Section of R code, I marked each table with specific code you can run.

# Sentiment Analysis

I first scrape the twitter within [-1, 85] days, and the scraped tweets is in /twitter folder. 

then I trained Delta TF-IDF model, the table for prediction accuracy will have to be obtained by running the actural python code, and the training dataset is in folder /datasets

then i use the trained algo to predict sentiments on actual tweet, and aggregate them on a daily frequency, result in folder /df_res

And that is pretty much it, let me know anything i can help by writing issue in github. 

