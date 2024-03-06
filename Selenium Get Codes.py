from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options

chrome_options = Options()
chrome_options.add_argument("--window-size=1920,1080")
chrome_options.add_experimental_option('useAutomationExtension', False)
DRIVER_PATH = '/Users/nicholasboffa/Downloads/chromedriver_mac64 (2)/Chromedriver'
driver = webdriver.Chrome(executable_path=DRIVER_PATH, chrome_options=chrome_options)

driver.get('https://programsandcourses.anu.edu.au/catalogue')

driver.find_element(By.XPATH, '//button[text()="Courses"]').click()

driver.find_element(By.XPATH, '//a[@data-template = "course-template"]').click()

codes = []
i = 0
try: 
    while i < 10000: #ASSUMING < 10000 courses; here in case there is
        code = driver.find_elements(By.XPATH, './/td[@class="catalogue-search-results__code"]')[i].text
        if code:
            codes.append(code)

        i += 1

    print(codes)
except:
    print(codes)



