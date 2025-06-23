## Code to scrape all course codes currently listed on the ANU's programs and courses 
# Uses selenium since page dynamically loads courses
# time.sleep() values might need to be tweaked depending on your internet speed to ensure everything loads
# Might need to run multiple times for it to work (might need to watch page for it to work? Maybe superstition)

import pandas as pd
import time

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

# -------- driver ----------
opts = Options()
opts.add_argument("--window-size=1920,1080")
# start fast; add flags back later as needed
driver = webdriver.Chrome(options=opts)   # no Service(), no manual path
wait   = WebDriverWait(driver, 10)

driver.get("https://programsandcourses.anu.edu.au/catalogue")

# -------- cookie banner ----------
try:
    wait.until(EC.element_to_be_clickable((By.ID, "onetrust-accept-btn-handler"))).click()
    wait.until(EC.invisibility_of_element_located((By.ID, "onetrust-banner-sdk")))
except TimeoutException:
    pass

# -------- helper ----------
def expand_section(action_suffix: str):
    """
    Click the 'Show all results…' link that belongs to the table-container
    whose data-action contains the given suffix (e.g. 'GetProgramsUnderGraduate').
    """
    container_xpath = f'//div[contains(@class,"table-container") and contains(@data-action,"{action_suffix}")]'
    link_xpath      = f'{container_xpath}//a[@data-template="program-template" and contains(text(),"Show all results")]'

    link = wait.until(EC.element_to_be_clickable((By.XPATH, link_xpath)))

    # scroll & JavaScript click avoid 'element intercepted' errors
    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", link)
    driver.execute_script("arguments[0].click();", link)

    # wait until the link becomes invisible (display:none) – signals rows are injected
    wait.until(EC.invisibility_of_element(link))

# -------- expand the four career sections ----------
for suffix in (
    "GetProgramsUnderGraduate",
    "GetProgramsPostGraduate",
    "GetProgramsResearch",
    "GetProgramsNonAward",
):
    try:
        expand_section(suffix)
    except TimeoutException:
        # link absent means the section was already fully expanded
        pass

print("All sections expanded – ready to scrape.")


program_rows = driver.find_elements(
    By.XPATH,
    '//td[@class="catalogue-search-results__code"]/parent::tr'
)

program_data = []
for row in program_rows:
    cols = row.find_elements(By.TAG_NAME, "td")
    code, title, study_type, career, atar, years, delivery_type = [c.text.strip() for c in cols[:7]]
    program_data.append([code, title, study_type, career, atar, years, delivery_type])


"""
#### SCRAPE COURSES

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

course_data = []
for row in course_rows:
    cols = row.find_elements(By.TAG_NAME, "td")
    code, title, term, career, units, delivery = [c.text.strip() for c in cols[:6]]
    course_data.append([code, title, term, career, units, delivery])

"""


# Write to DataFrame

#course_df = pd.DataFrame(course_data, columns=["code", "title", "session", "career", "units", "delivery_type"])
program_df = pd.DataFrame(program_data, columns = ["code", "title", "study_type", "career", "atar", "years", "delivery_type"])

# convert empty / whitespace-only strings to NA, then drop rows where *all* cells are NA
program_df = program_df.replace(r'^\s*$', pd.NA, regex=True).dropna(how="all")

program_df.to_csv("data/current/scrape_program_codes_df.csv", index=False)

"""
with open("data/current/course_codes.txt", "w") as f:
    f.writelines(code + "\n" for code in df["Code"])
"""

# Close browser
driver.quit()

