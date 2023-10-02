# import libraries
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.chrome.service import Service as ChromeService
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup as bs
import re as re
import time
import pandas as pd
import os

# data input
# The location of the web driver in your system, a username, and a password to log in with
## https://www.linkedin.com/
## first get the chrome webdriver on your local: https://bit.ly/3FHqvF7
## my webdriver path: /Users/satoshiido/Documents/chromedriver_mac_arm64/chromedriver
PATH = input("Enter the Webdriver path: ")
## input your linkedin account username or email address
USERNAME = input("Enter the username: ")
## input your linkedin account password
PASSWORD = input("Enter the password: ")
print(PATH)
print(USERNAME)
print(PASSWORD)


# web launch
# web documents of WebDriver for Chrome: https://chromedriver.chromium.org/getting-started
# how to include the ChromeDriver location in Mac OS System PATH: https://bit.ly/3lvBnz3
# when you get error: "“chromedriver” cannot be opened because the developer cannot be verified": https://bit.ly/3lqukb1
driver = webdriver.Chrome(PATH)
time.sleep(5)
driver.get("https://www.linkedin.com/uas/login")
time.sleep(5)

# tell the driver to login with the credentials provided above
email = driver.find_element(By.ID, 'username')
email.send_keys(USERNAME)
password = driver.find_element(By.ID, 'password')
password.send_keys(PASSWORD)
time.sleep(3)
password.send_keys(Keys.RETURN)
# driver.quit()

# eg: https://www.linkedin.com/in/idsts2670/
linkedin_page = input("Enter the Company or User Linkedin URL: ")
# if its company
company_name = linkedin_page[33:-1]
# if its user
user_name = linkedin_page[28:-1]


# if we want to check the person's posts, change driver.get as below
## for company profile -> driver.get(page + 'posts/')
## for personal profile -> driver.get(page + 'recent-activity/')
driver.get(linkedin_page)
SCROLL_PAUSE_TIME = 1.5
# get scroll height
last_height = driver.execute_script("return document.body.scrollHeight")


while True:
    # scroll down to bottom
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

    # wait to load page
    time.sleep(SCROLL_PAUSE_TIME)

    # calculate new scroll height and compare with last scroll height
    new_height = driver.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break
    last_height = new_height


# get current URL source code
pages = driver.page_source

# use beautiful Soup to get access tags
## warning: beautiful soup uses Xpath
linkedin_soup = bs(pages.encode("utf-8"), "html")
linkedin_soup.prettify()
containers = linkedin_soup.find_all("div", {"class":"pvs-entity pvs-entity--padded pvs-list__item--no-padding-in-columns"})
# for i in range(len(containers)):
#     print(containers[i].text)

# or simply use the below
time.sleep(5)
find = driver.find_elements(by = By.XPATH, value = '//div[@class="display-flex flex-column full-width align-self-center"]')

# input the extracted data into the list
l = []
for i in range(len(find)):
    l.append(find[i].text)

list = [*set(l)]
print(list)



