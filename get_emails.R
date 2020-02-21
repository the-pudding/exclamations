############## Package check and installation ###################
local({
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.r-project.org"
  options(repos = r)
})
check_inst_load <- function(pkg) {
  new <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  old <- pkg[(pkg %in% old.packages()[, "Package"])]
  inst <- union(new, old)
  if (length(inst))
    install.packages(inst, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages = c("tidyverse",
             "lubridate",
             "gmailr",
             "stringr",
             "base64url",
             "data.table")
check_inst_load(packages)
#################################################################
args = commandArgs(trailingOnly = TRUE) 

###### Data collection and organization ######
# Setup authorization
gm_auth_configure(path = "secret.json")
gm_auth(scopes = "readonly")

# Helper function to treat raw body data and remove threaded text

decode_and_trim <- function(data) {
  data %>%
    base64_urldecode() %>%
    gsub(
      "On .{0,5}[[:alpha:]]{3,4} [[:digit:]]{1,2}, [[:digit:]]{4}.*$|From:.*\r\nTo:.*$|On.+[[:digit:]]{2}.+[[:digit:]]{4}.+wrote.+$",
      "",
      .
    ) %>%
    return()
}

# Recursive helper function to find text part from message result
find_text_part <- function(part) {
  if (part$mimeType != "text/plain") {
    if (length(part$parts) > 0) {
      return(unlist(sapply(part$parts, find_text_part)))
    } else {
      return(NULL)
    }
  } else {
    if (!is.null(part$body$data))
      return(decode_and_trim(part$body$data))
    else
      return(NULL)
  }
}

# Creates data entry for each email
create_entry <- function(m_id, debug = FALSE) {
  if (debug)
    browser()
  message <- gm_message(m_id, format = "full")
  headers <- bind_rows(message$payload$headers)
  body <- find_text_part(message$payload)
  w <- strsplit(tolower(body), split = " |[[:cntrl:]]") %>%
    unlist() %>%
    gsub('[^[:print:]]|^.*http.*$', "", .) %>%
    gsub("(?!!)[[:punct:]]", "", ., perl = TRUE)
  s <-
    gsub("[\\.?!](?=\\s?[[:upper:][:cntrl:]])",
         "SENTENCE",
         body,
         perl = TRUE) %>%
    strsplit(split = "SENTENCE") %>%
    unlist()
  p <- strsplit(tolower(body), split = "(\r\n){2,}") %>%
    unlist()
  
  return(
    tibble(
      to = headers[headers$name == "To", 2][[1]] %>% gsub('^.*<(.*)>.*$', '\\1', .) %>% tolower(),
      multi = headers[headers$name == "To", 2][[1]] %>% grepl(",", .),
      from = headers[headers$name == "From", 2][[1]] %>% gsub('^.*<(.*)>.*$', '\\1', .) %>% tolower(),
      date = headers[headers$name == "Date", 2][[1]] %>% dmy_hms(),
      subject = headers[headers$name == "Subject", 2][[1]],
      words = sum(nchar(w) > 0),
      sentences = sum(nchar(s) > 0),
      paragraphs = sum(nchar(p) > 0),
      ex_rep = gsub("[^!]", " ", tolower(body)) %>%
        strsplit(split = " ") %>%
        unlist %>%
        discard(~ .x == "") %>%
        nchar() %>%
        paste0(collapse = "|"),
      ex_par = str_count(p, pattern = "!") %>% paste0(collapse = "|"),
      ex = str_count(w, pattern = "!") %>% sum
    )
  )
}

# Get message ids
m_ids <-
  gm_messages(
    search = NULL,
    num_results = as.integer(args[2]),
    include_spam_trash = FALSE
  ) %>%
  lapply(., function(el) {
    lapply(el$messages, function(ell) {
      return(ell$id)
    }) %>% return
  }) %>% unlist

# Generate the dataset from the ids and save to csv
output <- lapply(m_ids,
                 function(id) {
                   base::message(paste0("Getting email with message id: ", id))
                   tryCatch(
                     create_entry(id),
                     error = function(e) {
                       return(list(error = as.character(e)))
                     }
                   )
                 }) %>%
  bind_rows(., c(error = "test"))

base::message("Printing errors from getting and parsing emails")
output %>%
  filter(!is.na(error)) %>%
  select(error) %>%
  print()

if (args[3]) {
  base::message("Writing raw data file...")
  output %>%
    filter(is.na(error)) %>%
    select(-error) %>%
    write.csv(file = "email_data_raw.csv", row.names = FALSE)
  base::message("Success: written email_data_raw.csv")
}

base::message("Writing anonymized and processed data file...")
output %>%
  filter(is.na(error)) %>%
  select(-error) %>%
  mutate(
    type = ifelse(grepl(args[1], from), "out", "in"),
    grouping_id = ifelse(type == "in", as.character(from), as.character(to)),
    subject = gsub("Re: |RE: |Fwd: |FWD: ", "", subject)
  ) %>%
  group_by(type, grouping_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(chain = row_number()) %>%
  group_by(grouping_id) %>%
  filter(grepl("out", paste0(type, collapse = "")) &
           grepl("in", paste0(type, collapse = ""))) %>%
  ungroup() %>%
  mutate(
    subject = factor(subject, labels = length(levels)),
    to = factor(to, labels = length(levels)),
    from = factor(from, labels = length(levels)),
    grouping_id = factor(from, labels = length(grouping_id))
  ) %>%
  write.csv(file = "email_data.csv", row.names = FALSE)
base::message("Success: written email_data.csv")
