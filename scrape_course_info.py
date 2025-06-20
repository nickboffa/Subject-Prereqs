import requests
from bs4 import BeautifulSoup
import pandas as pd
from datetime import datetime

def get_valid_course_url(code, type): # type = earliest or latest
    base = "https://programsandcourses.anu.edu.au"
    current_year = datetime.now().year

    if type == "earliest":
        years_to_try = [current_year - 2, current_year - 1, current_year]
    elif type == "latest":
        years_to_try = [current_year, current_year - 1, current_year - 2]

    for year in years_to_try:
        url = f"{base}/{year}/course/{code}"
        try:
            resp = requests.head(url, allow_redirects=True, timeout=5)
            if resp.status_code == 200 and "Error/Index/404" not in resp.url:
                return url
        except requests.RequestException:
            continue  # skip to next year on timeout or error

    return None  # or raise an error/log warning

assessment_categories = {
    "exam": ["exam", "final exam"],
    "test/quiz": ["test", "quiz", "midsem", "mid-sem"],
    "essay/report": ["essay", "paper", "report", "writing"],
    "participation": ["participation", "contribution", "attendance", "engagement"],
    "oral/presentation": ["presentation", "oral", "talk", "pitch"]
}

def classify_assessment(name):
    name_lower = name.lower()
    for category, keywords in assessment_categories.items():
        if any(keyword in name_lower for keyword in keywords):
            return category
    return "other"

# Read course codes
with open("data/current/course_codes.txt", "r") as f:
    codes = [line.strip() for line in f if line.strip()]

records = []

for code in codes:  # Only process the first two codes
    url = get_valid_course_url(code, type="latest")
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')

    # Requisites section
    try:
        requisite_text = soup.find('div', class_='requisite').text
        requisite_text = ' '.join(requisite_text.split())
    except AttributeError:
        requisite_text = ''

    # Subject area/college/school
    college = school = area = ""
    for li in soup.find_all('li', class_='degree-summary__code'):
        heading = li.find('span', class_='degree-summary__code-heading')
        value = li.find('span', class_='degree-summary__code-text')

        if not heading or not value:
            continue

        heading_text = heading.text.strip().lower()
        value_text = value.text.strip()

        if "college" in heading_text:
            college = value_text.replace("ANU ", "")  # strip "ANU"
        elif "offered by" in heading_text:
            school = value_text.replace("ANU ", "")
        elif "areas of interest" in heading_text or "subject area" in heading_text:
            area = value_text

    if school == college:
        school = ""
    
    # Assessment summary link
    # Have to check previous years for assessment summary (might not be out for current year yet)
    summary_link = None
    old_url = get_valid_course_url(code, type="earliest")
    if old_url != url:
        assessment_page = requests.get(url)
        assessment_soup = BeautifulSoup(page.text, 'html.parser')
    else:
        assessment_soup = soup

    rows = assessment_soup.select("table.table-terms tbody tr")
    most_recent_date = None
    for row in rows:
        cols = row.find_all("td")
        if len(cols) < 7:
            continue
        date_str = cols[1].text.strip()
        try:
            date = datetime.strptime(date_str, "%d %b %Y")
        except ValueError:
            continue
        a_tag = cols[6].find("a")
        if a_tag and (most_recent_date is None or date > most_recent_date):
            most_recent_date = date
            summary_link = "https://programsandcourses.anu.edu.au" + a_tag.get("href")

    # Assessment type
    has_exam = has_test = has_essay = has_participation = has_oral = False
    max_weight = 0
    max_type = None
    max_name = ""
    if summary_link:
        try:
            summary_page = requests.get(summary_link, timeout=10)
            summary_soup = BeautifulSoup(summary_page.text, 'html.parser')

            # Find the Assessment Summary table
            rows = summary_soup.select('#assessment-summary + table.table-terms tbody tr')

            for row in rows:
                cells = row.find_all('td')
                if len(cells) < 2:
                    continue

                task_name = cells[0].text.strip()
                weight_text = cells[1].text.strip().replace('%', '')
                
                try:
                    weight = float(weight_text)
                except ValueError:
                    continue

                task_type = classify_assessment(task_name)

                # Set flags
                if task_type == "exam": has_exam = True
                if task_type == "test/quiz": has_test = True
                if task_type == "essay/report": has_essay = True
                if task_type == "participation": has_participation = True
                if task_type == "oral/presentation": has_oral = True

                if weight > max_weight:
                    max_weight = weight
                    max_type = task_type
                    max_name = task_name
        except:
            pass

    records.append({
        "code": code,
        "college": college,
        "school": school,
        "areas_of_interest": area,
        "exam": has_exam,
        "test_or_quiz": has_test,
        "essay_or_report": has_essay,
        "participation": has_participation,
        "oral_presentation": has_oral,
        "max_assessment_weight": max_weight if max_weight > 0 else None,
        "max_assessment_type": max_type,
        "max_assessment_name": max_name,
        "blurb": requisite_text,
        "summary_link": summary_link,
        "selt_link": f"https://unistats.anu.edu.au/internal/surveys/selt/learning/time-series/{code}_Time_Series_LRN.pdf"
    })

# Convert to DataFrame
df = pd.DataFrame(records)
print(df)

df.to_csv("data/current/scrape_course_info_df.csv", index=False)
