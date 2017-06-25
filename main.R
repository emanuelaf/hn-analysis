# load required packages
require(tidyverse)
require(httr)
require(tidytext)
require(stringr)

# import data to be used
common_words <- readr::read_csv("common_english.txt")
to_remove <- readr::read_csv("to_remove.txt")

# set the website
HN_API_url <- "https://hacker-news.firebaseio.com/v0/"

freelance_march_17 <- "13764729"
who_is_hiring_march_17 <- "13764728"

get_item_url <- function(id) {
  paste(HN_API_url, "item/", id, ".json", sep = "")
}

freelance_march_17_item <- httr::content(httr::GET( get_item_url(freelance_march_17) ) )
# we have 138 kids

who_is_hiring_march_17_item <- httr::content( httr::GET( get_item_url(who_is_hiring_march_17) ) )

populate_comments <- function(kids){
  comments <- tibble::tibble()
  for (kid in kids){
    comments <- kid %>% 
      get_item_url %>% 
      httr::GET() %>% 
      httr::content() %>% 
      tibble::as_data_frame() %>%
      dplyr::filter(!is.na(text) | !is.na(by)) %>%
      (function(df) {
        if (isTRUE(all.equal( c("by", "id", "parent", "text", "time", "type"),
                       names(df)))){
          rbind(comments, dplyr::select(df, by, id, parent, text, time, type))
        } else {
          comments
        }
    })
  }
  comments
}

comments_freelance <- populate_comments(freelance_march_17_item$kids) %>%
  mutate_all(tolower) 

comments_freelance <- comments_freelance %>%
  mutate(id = as.integer(id))

# there are warnings due to missing values. Let us see them:

comments_all <- as_tibble(unlist(freelance_march_17_item$kids))
names(comments_all) <- "id"

# some comments are left out

setdiff(comments_all, comments_freelance[,2])

# the reason is not as clear to me: sometimes it is ok because they were either
# deleted or they were empty but other times I do not know

# who is really seeking?
comments_freelance <- comments_freelance %>% 
  dplyr::mutate(seeking = stringr::str_detect(text, regex("seeking work")))


# get rid of useless text
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "\n", " ")
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "<p>", " ")

# harmonise backend
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "beck", " back ")
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "back-end", 
                                                "backend")
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "back end", 
                                                "backend")
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "backends", 
                                                "backend")
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "backed", 
                                                "backend")

# harmonise frontend
comments_freelance$text <- stringr::str_replace(comments_freelance$text, 
                                                "front end", 
                                                "frontend")

comments_freelance$text <- stringr::str_replace(comments_freelance$text, 
                                                "front-end", 
                                                "frontend")

# harmonise full stack
comments_freelance$text <- stringr::str_replace(comments_freelance$text, 
                                                "full stack", 
                                                "fullstack")

comments_freelance$text <- stringr::str_replace(comments_freelance$text, "full-stack", 
                                                "fullstack")

# system administration

comments_freelance$text <- stringr::str_replace(comments_freelance$text, "system administration", 
                                                "systemadministration")

comments_freelance$text <- stringr::str_replace(comments_freelance$text, "system administrator", 
                                                "systemadministrator")

# find ruby

comments_freelance$text <- stringr::str_replace(comments_freelance$text, " ruby", 
                                                " ruby ")

# unity
comments_freelance$text <- stringr::str_replace(comments_freelance$text, " unity", 
                                                " unity ")


# check I have done it right
# comments_freelance %>% 
#  filter(stringr::str_detect(text, regex(" full "))) %>% 
#  View()

# comments_freelance[comments_freelance$id==13773587,]$text

# isolate backend, frontend, fullstack (they might be close to )
# d[grepl("frontend", text, ignore.case=FALSE)] <- "frontend"

# keep only those that are seeking work 

comments_freelance <- comments_freelance %>% filter(seeking == TRUE)

# tidy text
word_count <- comments_freelance %>%
  tidytext::unnest_tokens(words, text) %>%
  dplyr::select(id_user = id, words) %>%
  dplyr::filter(!(words %in% common_words$common)) %>%
  dplyr::filter(!(words %in% to_remove$`to remove`)) %>%
  dplyr::group_by(id_user, words) %>%
  summarise(n = n())

total_words <- word_count %>%
  dplyr::ungroup() %>%
  dplyr::distinct(words)


words_by_id <- tidyr::spread(word_count, words, n)
# replace missing values with zeros
words_by_id[is.na(words_by_id)] <- 0

##### ---------  ADDITIONAL MANIPULATION

# ux developers are frontend!
words_by_id <- words_by_id %>%
  mutate(frontend = frontend + ux)

# saying frontend or iot is the same thing
words_by_id <- words_by_id %>%
  mutate(frontend = frontend + iot)

# saying mobile developer or ios is the same thing
words_by_id <- words_by_id %>%
  mutate(mobile = mobile + ios)


# saying android developer or mobile developer is the same thing
words_by_id <- words_by_id %>%
  mutate(mobile = mobile + android)

# saying unity developer or mobile developer is the same thing
words_by_id <- words_by_id %>%
  mutate(mobile = mobile + unity)

# system administration, system administration
# and sysadmin are the same thing
words_by_id <- words_by_id %>%
  mutate(sysadmin = sysadmin + systemadministration + systemadministrator)

words_by_id %>% select(starts_with("sy"))

words_by_id %>% select(starts_with("back"))

words_by_id %>% select(starts_with("full"))


# how many have been categorised?
words_by_id %>% 
  filter(backend > 0 | frontend >0 | fullstack > 0 | mobile > 0 |
           sysadmin > 0) %>% 
  select(id_user)

# define a character vector of backend technologies
backend_technologies <- c("python", "django", "bash",
                          "linux", "java", "perl", "postgresql", "mysql",
                          "php")

# define a character vector of frontend technologies
frontend_technologies <- c("html", "css", "javascript", "js")

words_by_id %>% 
  select(one_of(frontend_technologies)) %>%
  filter(html > 0 | css > 0 | javascript > 0 | js > 0) %>%
  mutate(frontend_assigned = html + css + javascript + js)

words_by_id %>% 
  select(one_of(backend_technologies)) %>%
  filter(python > 0 | django > 0 | bash > 0 |
         linux > 0 | java > 0 | perl > 0 | postgresql > 0 |
          mysql > 0 | php > 0) %>%
  mutate(backend_assigned = python + django + bash + linux + java + perl + postgresql + mysql + php)

words_by_id <- words_by_id %>% 
  mutate(frontend_assigned = html + css + javascript + js) %>%
  mutate(backend_assigned = python + django + bash + linux + 
           java + perl + postgresql + mysql + php + ruby + haskell + coq) %>%
  ungroup()

words_by_id <- words_by_id %>% 
  mutate(marketing_assigned = marketer)  %>%
  ungroup()

nessuno <- words_by_id %>% 
  select(c(id_user, backend, frontend, fullstack, 
           mobile, sysadmin, backend_assigned, 
           frontend_assigned, marketing_assigned)) %>%
  filter(backend == 0, frontend == 0, fullstack == 0, mobile == 0, 
         sysadmin == 0, backend_assigned == 0, frontend_assigned == 0,
         marketing_assigned == 0) %>%
  select(id = id_user) %>%
  left_join(comments_freelance) %>%
  ungroup() %>%
  select(id, text)



#comments_who_is_hiring <- populate_comments(who_is_hiring_march_17_item$kids)


