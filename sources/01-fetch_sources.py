#!/bin/python3
import re
from scraper import download, StatsNZ


print("Looking for latest subnational population estimates...")
XLS_PATTERN = "subnational-population-estimates-.*\.xlsx"
data_url = StatsNZ.get_latest_data_url("Subnational population estimates", XLS_PATTERN)
fn = re.match(r".*?([^\/]*\.xlsx)", data_url)[1]
download(data_url, f"sources/{fn}")

print("Looking for latest subnational population projections...")
XLS_PATTERN = "subnational-population-projections-.*\.xlsx"
data_url = StatsNZ.get_latest_data_url("Subnational population projections", XLS_PATTERN)
fn = re.match(r".*?([^\/]*\.xlsx)", data_url)[1]
download(data_url, f"sources/{fn}")

print("Done. You'll need to update filenames in 02-update_sources.R and delete the old files manually.")