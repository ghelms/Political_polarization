
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import re
# import string


# Since I started this project, folketingstidende.dk was redesigned.
# the list of full "referater" are called "tillæg f", and can now be found here
# https://www.folketingstidende.dk/da/tillæg/tillaeg-f


url = "https://www.folketingstidende.dk/da/till%C3%A6g/tillaeg-f?"\
      "session={}"\
      "&pageSize=1000"\
      "&pageNumber={}"


out_filename = "data/pdf_list.txt"
metadata_filename = "data/metadata.csv"
done_filename = ".state/visited.txt"
years = range(2019, 2021)  # gives just 2016, 2017

id_re = re.compile("([A-Z]+ \d+)")  # noqa
pdf_re = re.compile("(\S+\.pdf)")  # noqa
date_re = re.compile("(\d{2}-\d{2}-\d{4})")  # noqa
row_re = re.compile("([A-Z]+ \d+)?(.*?)(\d{2}-\d{2}-\d{4})")  # noqa


with open(done_filename, "r") as done_file:
    pages_scraped = done_file.read().splitlines()
    print("n of previously scraped pages: " + str(len(pages_scraped)))


print("Initializing browser engine")
options = Options()
options.headless=True
driver = webdriver.Firefox(executable_path="./geckodriver",
                           options=options)


# write header of output file
with open(metadata_filename, "w") as metadata:
    metadata.write("Samling;Nr;Titel;Dato;PDF;\n")

with open(out_filename, "a") as document_file:
    with open(metadata_filename, "a") as metadata:
        with open(done_filename, "a") as  done:
            for year in years:
                for sess in range(1, 3):  # 1 or 2 sessions per year
                    session = str(year) + str(sess)
                    for page_nr in range(1, 100):
                        this_url = url.format(session, page_nr)
                        if this_url in pages_scraped:
                            print("[Skipping] " + this_url)
                            continue
                        else:
                            print("[Scraping] " + this_url)

                        driver.get(this_url)
                        table_rows = driver.find_elements_by_css_selector(
                            "table tbody tr")

                        if len(table_rows) < 1:
                            # empty page, switch to some different parameters
                            break

                        # otherwise, we're good to go and can start picking pdfs
                        for row in table_rows:
                            cols = row.find_elements_by_css_selector("td")
                            nr = cols[0].text
                            titel = cols[1].text
                            dato = date_re.search(cols[2].text)
                            if dato:
                                dato = dato.group(1)
                                pdf = cols[3].find_element_by_css_selector("a")\
                                             .get_attribute("href")

                                document_file.write(pdf + "\n")
                                metadata.write(';'.join(
                                    [session, nr, titel, dato, pdf]) + ";\n")

                                done.write(this_url + "\n")
