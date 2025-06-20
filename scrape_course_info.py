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
    codes = [line.st