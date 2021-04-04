# dissertation_sentiment_analysis
This is a repo for my undergrade dissertation, the twitter scrape part is from https://github.com/israel-dryer/Twitter-Scraper/blob/main/twitter_scraper.py with some adjustment to fit my poor web condition, and the delta tf-idf is based on https://kgptalkie.com/amazon-and-imdb-review-sentiment-classification-using-spacy/. 

in order to run the code, in addition to download the code and have a working python environment (obviously), you need to have a selenium webdriver properly setup in your computer. In order to do that you can follow this link: https://selenium-python.readthedocs.io/installation.html, or video https://www.youtube.com/watch?v=9XAH_TvxwLg&t=29s
you also need a working twitter account & password, as suggested in the python script. 

Also,the twitter folder already have the scraped raw tweets, and the df_res folder already have the sentiment analysis result processed, the twitter search key words are "Comscore", "Rentrak", "Comscore Merger", "Rentrak Merger". 
# short-term event study methodology
the comscore.csv and rentrak.csv are the data collected from WRDS CRSP 
make sure the R code and these file are under the same root when you run the code
