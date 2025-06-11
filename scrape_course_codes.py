## Code to scrape all course codes currently listed on the ANU's programs and courses 
# Uses selenium since page dynamically loads courses
# time.sleep() values might need to be tweaked depending on your internet speed to ensure everything loads

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time

chrome_options = Options()
chrome_options.add_argument("--window-size=1920,1080")
chrome_options.add_experimental_option('useAutomationExtension', False)

service = Service('/Users/nicholasboffa/Downloads/chromedriver-mac-arm64/chromedriver')
driver = webdriver.Chrome(service=service, options=chrome_options)

driver.get('https://programsandcourses.anu.edu.au/catalogue')

wait = WebDriverWait(driver, 10)

# Click "Courses" button
wait.until(EC.element_to_be_clickable((By.XPATH, '//button[text()="Courses"]'))).click()

time.sleep(4)

# Click "Show all results..." link
wait.until(EC.element_to_be_clickable((By.XPATH, '//a[@data-template = "course-template"]'))).click()

time.sleep(6)

# Collect codes
codes = [el.text.strip() for el in driver.find_elements(By.XPATH, './/td[@class="catalogue-search-results__code"]') if el.text.strip()]

with open("/data/current/course_codes.txt", "w") as f:
    f.writelines(code + "\n" for code in codes)
