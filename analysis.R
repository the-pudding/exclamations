############## Package check and installation ###################
check_inst_load <- function(pkg) {
  new <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new)) install.packages(new, dependencies = TRUE, 
                                    repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}
packages = c("tidyverse",
             "lubridate",
             "gmailr",
             "stringr",
             "base64url",
             "parallel",
             "data.table")
check_inst_load(packages)
#################################################################

email_data <- read.csv("email_data.csv", header = TRUE) %>%
  mutate(date = as_date(date),
         epw = ex/words)

##### Plots 

email_data %>%
  group_by(type, chain) %>% 
  summarize(avg = mean(epw, na.rm = TRUE),
            count = n()) %>% 
  mutate(roll = frollmean(avg, 10, align = "center", na.rm = TRUE)) %>%
  pivot_longer(cols = c("avg", "roll"), names_to = "cat", values_to = "val") %>%
  filter(chain < 100) %>%
  ggplot(aes(x = chain, y = val, col = cat)) +
  geom_line() + 
  facet_grid(type~.) +
  ggtitle("Exclamation averages in email conversations over number of emails sent/recieved") +
  theme_bw()

email_data %>%
  group_by(grouping_id) %>% 
  arrange(date, .by_group=TRUE) %>% 
  mutate(chain = row_number()) %>% 
  ggplot(aes(x = chain, y = epw, col = grouping_id)) +
  geom_line() + 
  facet_grid(type~.) +
  guides(color = FALSE) +
  ggtitle("Exclamations in email conversations over number of emails sent/recieved colored by each email address") +
  theme_bw()

# ggplot(email_data, aes(x = log(1 + epw), fill = type)) +
#   geom_histogram(
#     aes(y = ..density..),
#     color = "grey",
#     bins = 100,
#     alpha = 0.5,
#     position = "identity"
#   ) +
#   theme_bw()

ggplot(email_data, aes(x = date, y = epw, color = type)) +
  geom_point(alpha = 0.1) +
  facet_grid(type ~ .) +
  guides(color = FALSE) +
  ggtitle("Exclamations for each email over time") +
  theme_bw()

##### Frequency of exclamation repeats
create_pos <- function(vec){
  vec %>% as.character() %>%
  lapply(function(el){
    x <- strsplit(el, split = "[|]") %>%
      unlist() %>%
      as.integer()
    y <- (seq_along(x)-0.5)/length(x)
    rep(y, x)
  }) %>%
    unlist() %>%
    list() %>%
    return()
}

create_rep_dist <- function(vec){
  paste0(vec, collapse="|") %>%
    strsplit(split = "[|]") %>%
    unlist() %>%
    as.integer() %>%
    list() %>% 
    return()
}

a <- email_data %>%
  group_by(type) %>%
  summarize(dist = create_rep_dist(ex_rep))

b <- email_data %>%
  group_by(type) %>%
  filter(nchar(as.character(ex_par)) >= 5) %>%
  summarize(dist = create_pos(ex_par)) 


data.frame(type = "in", count = a$dist[[1]]) %>%
  rbind(data.frame(type = "out", count = a$dist[[2]])) %>%
  ggplot(aes(x = count, fill = type)) +
  geom_bar(aes(y =..prop..), position=position_dodge()) + 
  xlim(0.5, 5.5) +
  ggtitle("Percentage of repeated exclamations") +
  theme_bw()

data.frame(type = "in", para = b$dist[[1]]) %>%
  rbind(data.frame(type = "out", para = b$dist[[2]])) %>% 
  ggplot(aes(x = para, fill = type)) +
  geom_histogram(aes(y =..density..), position="identity", alpha = 0.4, bins = 15) + 
  ggtitle("Appearance of exclamations across email paragraphs") +
  theme_bw()

