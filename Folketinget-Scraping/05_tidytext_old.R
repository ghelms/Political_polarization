library(tidyverse)
library(lubridate)
# library(udpipe)
library(stopwords)
library(groupdata2)
library(fuzzyjoin)


# reshape the segmented data
# dl <- udpipe_download_model(language = "danish")

title_re_paren = "(?<=\\().*(?=\\))"
title_re_noparen = ".*"
name_re = "[\\w\\s-\\.]+"

getwd()

# load in metadata
meta = read_delim("./Folketinget-Scraping/data/metadata.csv", ";", col_types = cols()) %>%
    mutate(id = tools::file_path_sans_ext(basename(PDF))) %>%
    select(-PDF, -X6)

# testing functions on random text-file
text_file = read_delim("./Folketinget-Scraping/data/segmented/20191_M4_referat.txt", delim = ";", col_types = cols()) %>% 
    filter(complete.cases(reason))

tidy_text <- function(filename) {
    # print to screen the filename being formatted
    cat(paste0("[ ] Tidying ", filename, "\n"))
    # read filename and filter missing incomplete values in "reason"
    d = read_delim(filename, delim = ";", col_types = cols()) %>%
        filter(complete.cases(reason))

    # færre end tre rækker i dokumentet returnerer en 1x1 df med NA i value.
    if (nrow(d) < 3) return(data.frame(NA))
    
    # strengen "name" i kolonnen "reason" returnerer en 1x1 df med NA i value.
    if (!any(str_detect(d$reason, "name"))) return (data.frame(NA))

    d = d %>%
        filter(split > 0) %>%
        mutate(value = case_when(
                    # hvis reason er title_name igangsættes det efterfølgende ifelse-statement
                    reason == "title_name" ~ ifelse(
                                    # test
                                    str_detect(value, "\\(|\\)"),
                                    # if true
                                    str_extract(value, title_re_paren),
                                    # if false
                                    str_extract(value, title_re_noparen)),
                    # hvis reason er name_party
                    reason == "name_party" ~ str_extract(value, name_re),
                    # hvis reason er Time
                    reason == "Time" ~ value),
               reason = ifelse(reason == "Time", "Time", "Name"),
               # remove whitespace from value column
               value = trimws(value)) %>%
        # instead of having "Title" or "Name" in reason-col and corresponding title and name in value-col,
        # there will now be a column called "Title" and a column called "Name". 
        spread(reason, value) %>%
        # deselect split-column
        select(-split)

    if ("Time" %in% colnames(d)) {
        d = d %>%
            # latest value in each column is repeated until next value (filling in missing values)
            fill(Time, Name) %>%
            # time format changed to hours and minutes
            mutate(Time = hm(Time))
    } else {
        d$Time = hm(NA)
        }
    
    d = d %>%
        # only keep rows where both text and Name is filled in.
        filter(complete.cases(text), complete.cases(Name)) %>%
        # adding column "id" based on the inputted filename
        mutate(id = tools::file_path_sans_ext(basename(filename))) %>%
        as_data_frame()

    return(d)
    
}

test_function = tidy_text("./Folketinget-Scraping/data/segmented/20201_M101_referat.txt")

files = list.files("./Folketinget-scraping/data/test", "*.txt", full.names = TRUE)

data = map_dfr(files, tidy_text) %>%
    ## bind_rows() %>%
    # adding metadata
    right_join(meta, by = "id") %>%
    # adding column "Date" with combined Time and Dato, formatted using "orders". 
    mutate(Date = parse_date_time(str_c(Dato, Time, sep = " "),
                                  orders = c("dmy HMS", "dmy MS", "dmy S"))) %>%
    #remove missing values
    filter(complete.cases(Date)) %>%
    # sort based on date
    arrange(Date) %>%
    mutate(speaker_id = ifelse(lag(Name) != Name, 1, 0) %>%
                # taking care of NA in top row
                coalesce(0) %>%
                # cumulative sum
                cumsum()) %>%
    # har ikke styr på hvad der helt præcist sker her, noget med at samle flere rækker i én
    group_by(Name, id, Samling, Nr, Titel, Dato, speaker_id) %>%
    summarise(text = str_c(text, collapse = " "),
              Date = min(Date)) %>% ungroup() %>%
    mutate(doc_id = as.character(as.integer(as.factor(str_c(speaker_id, Name, Date))))) %>%
    ungroup()


## clean up some common parsing errors in the names
data = data %>%
    mutate(Name = Name %>%
               str_replace_all("^\\!", "I") %>%
               str_replace_all("[\"(){}\\[\\]\\\\\\'»]", "") %>%
               str_replace_all("\\d+", "") %>%
               str_replace_all("^\\-", "") %>%
               str_replace("Forste næstformand", "Første næstformand") %>%
               str_replace("Formandell", "Formanden") %>%
               str_replace("Fôrmanden", "Formanden") %>%
               str_replace("Formandøn", "Formanden") %>%
               str_replace("Deli fg. formand", "Den fg. formand") %>%
               trimws())


###################
# handle titles like "Anden næstformand"
read_csv2("./Folketinget-scraping/data/folketing_leaders.csv") %>%
    # reformatting using lubridate (date formatting)
    mutate(from = dmy(fra),
           to = dmy(til),
           Title = "Formand") %>%
    rename(Navn = formand) %>%
    # deselecting initial columns
    select(-fra, -til, -levetid) ->
    # saving output in df "titles_tmp"
    titles_tmp


read_tsv("./Folketinget-scraping/ministerposter.tsv") %>%
    # reformatting using lubridate
    mutate(from = dmy(Start),
           to = dmy(Slut)) %>%
    # deselecting initial columns
    select(-Start, -Slut) %>%
    rename(Title = Ministerpost) %>%
    bind_rows(titles_tmp) %>%
    mutate(period = interval(from, to)) ->
    titles
    

titles[name == titles$Title]


find_name_from_title = function(name, date) {
    # matcher fx "formand | formanden". Resultatet må være en df der indeholder rækker der matchede
    d = titles[name == titles$Title | name == str_c(titles$Title, "en"),]
    # correct date range (the title switches hands)
    # kun hvis d er en df giver det her mening, så kan han indeksere d og matche baseret på dato
    d = d[date %within% d$period,]
    n = d$Navn
    p = d$parti
    t = name

    # character(0) er en character-vektor med en længde på 0. Her testes få noget a la NA værdi i name.
    if (identical(n, character(0))) {
        
        n = name
        t = NA
        p = NA
    }
    
    return(data.frame(Title = t, Name = n, Parti = p, stringsAsFactors = FALSE))    
}


title_subset = data %>%
    #sample_n(10) %>% 
    # only keep distinct rows (men hvorfor Name vs Dato? Måske nogle fejl han har oplevet)
    distinct(Name, Dato) %>%
    # ny formattering af Dato vha. lubridate funktion
    mutate(Dato2 = dmy(Dato))  %>%
    # map2 bruges vel til at fodre de to inputs Name og Dato2 til funktionen find_name_from_title
    mutate(Name = map2(Name, Dato2, find_name_from_title)) %>% unnest(Name) %>%
    select(Dato, Name, Title, Parti)

data2 = left_join(data, title_subset, by = c("Name" = "Title", "Dato")) %>%
    rename(Title = Name, Name = Name.y) %>%
    mutate(Name = ifelse(is.na(Name), Title, Name))


#################
# load in data on who's in which party
cat("[ ] Combining with party data\n")
ft_members = read_delim("./Folketinget-Scraping/data/folketing_members.csv", ";", col_names =FALSE, col_types = cols())
names(ft_members) = c("Name", "Parti", "Year")

ft_members_copy = ft_members

ft_members = ft_members %>%
    ## mutate(Parti = str_extract(Parti, "\\w+") ) %>%
    group_by(Name, Parti) %>%
    # "Year" becomes the minimum value of Year per member
    summarise(Year = min(Year)) %>% ungroup()

ft_members = ft_members %>%
    tidyr::expand(Name, Year = min(Year):max(Year)) %>%
    left_join(ft_members, by = c("Name", "Year")) %>%
    arrange(Name, Year) %>%
    fill(Parti) %>%
    filter(complete.cases(Parti))


data3 = data2 %>%
    mutate(Year = year(Date)) %>%
    left_join(ft_members, by = c("Name", "Year")) %>%
    mutate(Parti = ifelse(is.na(Parti.x), Parti.y, Parti.x)) %>%
    select(-Parti.x, -Parti.y)

## try to catch some more names
## if they don't have party affilation data by now,
## the names are probably abbreviated.
## lets try some heuristics.

test_ft = ft_members_copy %>%
    mutate(Year = map(Year, ~ .x:(.x+3))) %>%
    unnest()



weird_names = filter(data3, is.na(Parti)) %>%
    distinct(Name, Year) %>%
    arrange(Name, Year)

## test_ft = filter(ft_members_copy, str_detect(Name, "Christensen"))

less_weird_names = fuzzy_join(weird_names, test_ft,
           by = c("Year", "Name"),
           match_fun = list(
               function(x, y) {
                   x == y},
               function(x, y) {
                   str_detect(y, x)})) %>%
    rename(realname = Name.y, Year = Year.y, Name = Name.x) %>%
    select(-Year.x)


data3 = left_join(data3, less_weird_names, by = c("Year", "Name")) %>%
    mutate(Parti = ifelse(is.na(Parti.x), Parti.y, Parti.x),
           Name = ifelse(is.na(realname), Name, realname)) %>%
    select(-Parti.x, -Parti.y, -realname) %>%
    distinct()
    

##            match_fun = list(`==`, match_fun))

       


## bit of memory management
## rm(data)
## rm(data2)

cat("[ ] Combining with election period data\n")

periods = tribble(~Period,     ~StartDate,
                  "1953-1957", "22-09-1953",
                  "1957-1960", "14-05-1957",
                  "1960-1964", "15-11-1960",
                  "1964-1966", "22-09-1964",
                  "1966-1968", "22-11-1966",
                  "1968-1971", "23-01-1968",
                  "1971-1973", "21-09-1971",
                  "1973-1975", "04-12-1973",
                  "1975-1977", "09-01-1975",
                  "1977-1979", "15-02-1977",
                  "1979-1981", "23-10-1979",
                  "1981-1984", "08-12-1981",
                  "1984-1987", "10-01-1984",
                  "1987-1988", "08-09-1987",
                  "1988-1990", "10-05-1988",
                  "1990-1994", "12-12-1990",
                  "1994-1998", "21-09-1994",
                  "1998-2001", "11-03-1998",
                  "2001-2005", "20-11-2001",
                  "2005-2007", "08-02-2005",
                  "2007-2011", "24-10-2007",
                  "2011-2015", "15-09-2011",
                  "2015-2019", "18-06-2015",
                  "2019-"    , "05-06-2019") %>%
    mutate(StartDate = dmy(StartDate))

find_period <- function(date) {
    if (is.na(date)) {return(NA)}
    for (i in nrow(periods):1) {
        thisrow = periods[i, ]
        if (date >= thisrow$StartDate) {
            return (thisrow$Period)
        }
    }
    ## nothing found
    return(NA)
    }

cat("[ ] Matching dates with election periods\n")
data3 = data3 %>%
    mutate(Period = map_chr(Date, find_period),
           Period = ifelse(Parti == "ALT", "2015-2017", Period))


## cat("[ ] Combining with minister data\n")

## lookup_minister = function(name, date) {
##     res = filter(minister,
##            Navn == !!name,
##            date %within% Period) %>%
##         pluck("Ministerpost")

##     return(ifelse(is.null(res), NA, res))
##     }


## data3 = data3 %>%
##     mutate(Ministerpost = map2_chr(Name, Date, lookup_minister))


#################
cat("[ ] pre-preprocessing text\n")
data3$text = data3$text %>%
    str_replace_all("\\b[:alpha:] [:digit:]+", " ") %>% # lovforslag
    str_to_lower(locale = "da") %>%
    str_replace_all(str_c("[–_/()\\s'»$&+", '"', "]+"), " ")

data3 = filter(data3, str_detect(text, "\\S"), nchar(text) > 20)


## data3 %>%
##     sample_n(10) %>%
##     select(text) %>% pluck(1) %>%
##     write_lines("models/for_udpipe")


## data3 %>%
##     select(-text, -speaker_id) %>%
##     write_csv("data/tidy_text.csv")


#################
cat("[ ] Udpipe\n")
# ud = udpipe_load_model(file = list.files(pattern = "danish-ud.*udpipe"))

# d = sample_n(data3, 5000)
# d = data3

# parallel processing
## cluster <- create_cluster(4) %>%
##     cluster_library("tidyverse") %>%
##     cluster_library("udpipe")

lemma = data3 %>%
    arrange(doc_id) %>%
    groupdata2::group(100)

lemma %>%
    select(-text, -speaker_id) %>%
    write_csv("./Folketinget-Scraping/data/tidy_metadata.csv")

write_csv(lemma, "./Folketinget-Scraping/data/folketinget_1953_2019_raw.csv")

 
lemma %>%
    split(.$.groups) %>%
    walk(~(write_lines(.$text, str_c("./Folketinget-Scraping/data/to_udpipe/", unique(.$.groups)))))
# it works!

##############

