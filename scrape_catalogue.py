## Code to scrape all course codes currently listed on the ANU's programs and courses 
# Uses selenium since page dynamically loads courses
# time.sleep() values might need to be tweaked depending on your internet speed to ensure everything loads
# Might need to run multiple times for it to work (might need to watch page for it to work? Maybe superstition)

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import pandas as pd
import time

chrome_options = Options()
chrome_options.add_argument("--window-size=1920,1080")
chrome_options.add_experimental_option('useAutomationExtension', False)

service = Service('/Users/nicholasboffa/Downloads/chromedriver-mac-arm64/chromedriver')
driver = webdriver.Chrome(service=service, options=chrome_options)

driver.get('https://programsandcourses.anu.edu.au/catalogue')

print('1')
wait = WebDriverWait(driver, 10)
print("hi")

# Click "Courses" button
wait.until(EC.element_to_be_clickable((By.XPATH, '//button[text()="Courses"]'))).click()

print("2")
time.sleep(8)

# Click "Show all results..." link
wait.until(EC.element_to_be_clickable((By.XPATH, '//a[@data-template = "course-template"]'))).click()

print("3")
time.sleep(6)

course_rows = driver.find_elements(
    By.XPATH,
    '//td[@class="catalogue-search-results__code"]/parent::tr'
)

data = []
for row in course_rows:
    cols = row.find_elements(By.TAG_NAME, "td")
    code, title, term, career, units, delivery = [c.text.strip() for c in cols[:6]]
    data.append([code, title, term, career, units, delivery])

# Write to DataFrame
df = pd.DataFrame(data, columns=["code", "title", "session", "career", "units", "delivery_type"])

# convert empty / whitespace-only strings to NA, then drop rows where *all* cells are NA
df = df.replace(r'^\s*$', pd.NA, regex=True).dropna(how="all")

df.to_csv("data/current/scrape_course_codes_df.csv", index=False)

"""
with open("data/current/course_codes.txt", "w") as f:
    f.writelines(code + "\n" for code in df["Code"])
"""

# Close browser
driver.quit()
