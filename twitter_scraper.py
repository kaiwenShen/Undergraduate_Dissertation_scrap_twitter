import csv
from time import sleep
from msedge.selenium_tools import Edge, EdgeOptions
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from selenium.common import exceptions


def create_webdriver_instance():
    options = EdgeOptions()
    options.use_chromium = True
    driver = Edge(options=options)
    return driver


def login_to_twitter(username, password, driver):
    url = 'https://twitter.com/login'
    sleep(10)
    try:
        driver.get(url)
        xpath_username = '//input[@name="session[username_or_email]"]'
        WebDriverWait(driver, 10).until(expected_conditions.presence_of_element_located((By.XPATH, xpath_username)))
        uid_input = driver.find_element_by_xpath(xpath_username)
        uid_input.send_keys(username)
    except exceptions.TimeoutException:
        print("Timeout while waiting for Login screen")
        return False

    pwd_input = driver.find_element_by_xpath('//input[@name="session[password]"]')
    pwd_input.send_keys(password)
    try:
        pwd_input.send_keys(Keys.RETURN)
        url = "https://twitter.com/home"
        WebDriverWait(driver, 10).until(expected_conditions.url_to_be(url))
    except exceptions.TimeoutException:
        print("Timeout while waiting for home screen")
    return True


def find_search_input_and_enter_criteria(search_term, driver):
    sleep(20)
    xpath_search = '//input[@aria-label="Search query"]'
    search_input = driver.find_element_by_xpath(xpath_search)
    search_input.send_keys(search_term)
    search_input.send_keys(Keys.RETURN)
    return True


def change_page_sort(tab_name, driver):
    """Options for this program are `Latest` and `Top`"""
    sleep(30)
    tab = driver.find_element_by_link_text(tab_name)
    tab.click()
    xpath_tab_state = f'//a[contains(text(),\"{tab_name}\") and @aria-selected=\"true\"]'


def generate_tweet_id(tweet):
    return ''.join(tweet)


def scroll_down_page(driver, last_position, num_seconds_to_load=3, scroll_attempt=0, max_attempts=10):
    """The function will try to scroll down the page and will check the current
    and last positions as an indicator. If the current and last positions are the same after `max_attempts`
    the assumption is that the end of the scroll region has been reached and the `end_of_scroll_region`
    flag will be returned as `True`"""
    end_of_scroll_region = False
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    sleep(num_seconds_to_load)
    curr_position = driver.execute_script("return window.pageYOffset;")
    if curr_position == last_position:
        if scroll_attempt < max_attempts:
            end_of_scroll_region = True
        else:
            scroll_down_page(last_position, curr_position, scroll_attempt + 1)
    last_position = curr_position
    return last_position, end_of_scroll_region


def save_tweet_data_to_csv(records, filepath, mode='a+'):
    header = ['PostDate', 'TweetText', 'ReplyCount', 'RetweetCount', 'LikeCount']
    with open(filepath, mode=mode, newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        if mode == 'w':
            writer.writerow(header)
        if records:
            writer.writerow(records)


def collect_all_tweets_from_current_view(driver, lookback_limit=25):
    """The page is continously loaded, so as you scroll down the number of tweets returned by this function will
     continue to grow. To limit the risk of 're-processing' the same tweet over and over again, you can set the
     `lookback_limit` to only process the last `x` number of tweets extracted from the page in each iteration.
     You may need to play around with this number to get something that works for you. I've set the default
     based on my computer settings and internet speed, etc..."""
    page_cards = driver.find_elements_by_xpath('//div[@data-testid="tweet"]')
    if len(page_cards) <= lookback_limit:
        return page_cards
    else:
        return page_cards[-lookback_limit:]


def extract_data_from_current_tweet_card(card):
    # for this project, we dont necessarily need username or user handle, so we comment this part out

    # try:
    #     user = card.find_element_by_xpath('.//span').text
    # except exceptions.NoSuchElementException:
    #     user = ""
    # except exceptions.StaleElementReferenceException:
    #     return
    #
    # try:
    #     handle = card.find_element_by_xpath('.//span[contains(text(), "@")]').text
    # except exceptions.NoSuchElementException:
    #     handle = ""

    try:
        """
        If there is no post date here, there it is usually sponsored content, or some
        other form of content where post dates do not apply. You can set a default value
        for the postdate on Exception if you which to keep this record. By default I am
        excluding these.
        """
        postdate = card.find_element_by_xpath('.//time').get_attribute('datetime')
    except exceptions.NoSuchElementException:
        return
    # we include both tweets and comments
    try:
        _comment = card.find_element_by_xpath('.//div[2]/div[2]/div[1]').text
    except exceptions.NoSuchElementException:
        _comment = ""

    try:
        _responding = card.find_element_by_xpath('.//div[2]/div[2]/div[2]').text
    except exceptions.NoSuchElementException:
        _responding = ""
    tweet_text = _comment + _responding
    # collect reply, likes and retweets data in case we need them.
    try:
        reply_count = card.find_element_by_xpath('.//div[@data-testid="reply"]').text
    except exceptions.NoSuchElementException:
        reply_count = ""
    try:
        retweet_count = card.find_element_by_xpath('.//div[@data-testid="retweet"]').text
    except exceptions.NoSuchElementException:
        retweet_count = ""
    try:
        like_count = card.find_element_by_xpath('.//div[@data-testid="like"]').text
    except exceptions.NoSuchElementException:
        like_count = ""

    # tweet = (user, handle, postdate, tweet_text, reply_count, retweet_count, like_count)
    tweet = (postdate, tweet_text, reply_count, retweet_count, like_count)
    return tweet


def main(username, password, search_term, filepath, page_sort='Latest'):
    save_tweet_data_to_csv(None, filepath, 'w')  # create file for saving records
    last_position = None
    end_of_scroll_region = False
    unique_tweets = set()

    driver = create_webdriver_instance()  # put msedge selenium in system path
    logged_in = login_to_twitter(username, password, driver)
    if not logged_in:
        return

    search_found = find_search_input_and_enter_criteria(search_term, driver)  # u will have to maximize web-browser
    if not search_found:
        return

    change_page_sort(page_sort, driver)

    # while not end_of_scroll_region:
    # is the normal (and correct)way, but if you experience poor internet connection (like me)
    # set while condition to true
    # sometime internet issue, twitter will alert "something wrong, please try again"
    # so while true, manually click the refresh button and the loop will carry on

    while True:
        sleep(2)
        cards = collect_all_tweets_from_current_view(driver)
        for card in cards:
            try:
                tweet = extract_data_from_current_tweet_card(card)

            except exceptions.StaleElementReferenceException:
                continue
            if not tweet:
                continue

            tweet_id = generate_tweet_id(tweet)
            if tweet_id not in unique_tweets:
                unique_tweets.add(tweet_id)
                save_tweet_data_to_csv(tweet, filepath)
        last_position, end_of_scroll_region = scroll_down_page(driver, last_position)

    print("\nfinished scraping, closing the window")
    # driver.quit()


if __name__ == '__main__':
    usr = ""  # your user account
    pwd = ""  # your password
    path = 'twitter/tweets_comscore_merger_scrape_st.csv'
    # using advanced twitter search
    # we only want english posts, hence we add on the tag lang:en
    # until:xxxx-xx-xx since:xxxx-xx-xx is the time frame
    term = 'comscore merger lang:en until:2016-01-30 since:2015-09-28'

main(usr, pwd, term, path)
