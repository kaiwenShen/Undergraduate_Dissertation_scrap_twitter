# using TF-IDF
# measure of originality of a word by comparing the number of times a word appears in a doc with the number of docs the
# word appears in
# term frequency * inverse document frequency
import spacy
from sklearn.metrics import classification_report
from spacy.lang.en.stop_words import STOP_WORDS
import pandas as pd
import string
from sklearn_deltatfidf import DeltaTfidfVectorizer
from sklearn.svm import LinearSVC
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import re
import datetime

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

nlp = spacy.load('en_core_web_sm')


def text_data_cleaning(sentence):
    doc = nlp(sentence)
    stopwords = list(STOP_WORDS)
    punct = string.punctuation
    hashtag_at = re.compile('[#@$]')
    tokens = []
    for token in doc:
        if token.lemma_ != "-PRON-":
            temp = token.lemma_.lower().strip()
        else:
            temp = token.lower_
        tokens.append(temp)

    cleaned_tokens = []
    for token in tokens:
        if token not in stopwords and token not in punct and len(token) < 15 and hashtag_at.search(token) is None:
            cleaned_tokens.append(token)
    return cleaned_tokens


# load training data
data_yelp = pd.read_csv('datasets/yelp_labelled.txt', sep='\t', header=None)
data_imdb = pd.read_csv('datasets/imdb_labelled.txt', sep='\t', header=None)
data_amazon = pd.read_csv('datasets/amazon_cells_labelled.txt', sep='\t', header=None)

columns_name = ['Review', 'Sentiment']
data_yelp.columns = columns_name
data_imdb.columns = columns_name
data_amazon.columns = columns_name

data = data_yelp.append([data_amazon, data_imdb], ignore_index=True)

# v = DeltaTfidfVectorizer(tokenizer=text_data_cleaning)


X = data['Review']
y = data['Sentiment']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

data = []
labels = []
for item in X_train:
    data.append(item)

for item in y_train:
    labels.append(item)

v = DeltaTfidfVectorizer()
v.fit_transform(data, labels)
clf = Pipeline([
    ('vectorizer', DeltaTfidfVectorizer()),
    ('clf', LinearSVC())
])
# clf = Pipeline([('vectorizer', TfidfVectorizer()), ('clf', LinearSVC())])
clf.fit(data, labels)

y_pred = clf.predict(X_test)
print(classification_report(y_test, y_pred))


# comscore_search_file = r'tweet_scrap/tweets_comscore_scrape.csv'
# comscore_merger_search_file = r'tweet_scrap/tweets_comscore_merger_scrape.csv'


def readfile(filename):
    df = pd.read_csv(filename, sep=',', header=None)
    df = df.iloc[1:]
    df = df.drop([2, 3, 4], axis=1)
    df.columns = ['Time', 'Tweet']
    df = df.reset_index(drop=True)
    return df


comscore_merger_search_file = r"tweets_rentrak_merger_scrape_st.csv"
comscore_search_file = r"tweets_rentrak_scrape_st.csv"

comscore_merger = readfile(comscore_merger_search_file)
comscore_search = readfile(comscore_search_file)


# print(comscore_merger.head)


def predict_tweets(df):
    res = []
    for line in df.Tweet:
        res.append(clf.predict([str(line)]))
    res = pd.DataFrame(res)
    res = pd.concat([df, res], axis=1, ignore_index=True)
    res.columns = ['Time', 'Tweet', 'Sentiment']
    res = res.reset_index(drop=True)
    return res


res_merger = predict_tweets(comscore_merger)
res_search = predict_tweets(comscore_search)


# print(res_search.head())


def process_day_sentiment(res):
    res.Time = pd.to_datetime(res.Time).dt.date
    size = res.groupby('Time').size()
    total_positive = res.groupby('Time')['Sentiment'].sum()
    day_sentiment = pd.concat([size, total_positive], axis=1, ignore_index=True)
    day_sentiment.columns = ['num_of_tweets', 'num_of_positive']
    day_sentiment['pct'] = day_sentiment.num_of_positive / day_sentiment.num_of_tweets
    day_sentiment.reset_index(inplace=True)
    return day_sentiment


# post analysis data processing
day_sentiment_merger = process_day_sentiment(res_merger)
day_sentiment_search = process_day_sentiment(res_search)


def line_with_bar(day_sentiment, plot_title, y_1, y_2):
    fig = plt.figure(figsize=(15, 5))
    ax1 = fig.add_subplot(111)
    ax1.plot(day_sentiment.Time, day_sentiment.pct, label='Pct')
    ax1.set(ylabel=y_1)
    ax1.legend(bbox_to_anchor=(1.12, 1))
    ax2 = ax1.twinx()
    ax2.bar(day_sentiment.Time, day_sentiment.num_of_tweets, color='red', alpha=0.3, label='Tweets')
    ax2.grid(False)
    ax2.set(ylabel=y_2)
    ax2.legend(bbox_to_anchor=(1.131, 0.9))
    plt.xlabel("Date")
    plt.title(plot_title)
    plt.show()


# lineplot with barplot
line_with_bar(
    day_sentiment_merger, 'Percentage of Positive Sentiment Towards Merger', 'Percentage of Positive tweets',
    'Numbers of tweets Collected')

line_with_bar(
    day_sentiment_search, 'Percentage of Positive Sentiment Towards ComScore', 'Percentage of Positive tweets',
    'Numbers of tweets Collected')


def stacked_bar(day_sentiment, title, stack1, stack2):
    fig, ax = plt.subplots(figsize=(15, 5))
    day_sentiment['num_of_negative'] = day_sentiment.num_of_tweets - day_sentiment.num_of_positive
    ax.bar(day_sentiment.Time, day_sentiment.num_of_negative, 0.35, label=stack1)
    ax.bar(day_sentiment.Time, day_sentiment.num_of_positive, 0.35, label=stack2)
    ax.set_ylabel('Numbers of Tweets')
    ax.set_title(title)
    ax.legend()
    plt.show()


# stacked bar
stacked_bar(day_sentiment_merger, 'Positive and Negative Tweets Towards Rentrak Merger', 'Negative Tweets',
            'Positive Tweets')
stacked_bar(day_sentiment_search, 'Positive and Negative Tweets Towards Rentrak', 'Negative Tweets', 'Positive Tweets')


# export the pandas dataframe into csv
# but first we need to convert time into interger


def to_integer(dt_time):
    return 10000 * dt_time.year + 100 * dt_time.month + dt_time.day


i = 0
for time in day_sentiment_search.Time:
    day_sentiment_search.Time.loc[i] = to_integer(time)
    i += 1

i = 0
for time in day_sentiment_merger.Time:
    day_sentiment_merger.Time.loc[i] = to_integer(time)
    i += 1
# warnings but works

day_sentiment_search.to_csv(r"df_res/day_sentiment_search_rt.csv", index=False)
day_sentiment_merger.to_csv(r"df_res/day_sentiment_merger_rt.csv", index=False)
