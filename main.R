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
comments_freelance %>% 
  dplyr::mutate(seeking = stringr::str_detect(text, regex("seeking work"))) %>% View()

# get rid of useless text
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "\n", 
                                                " ")
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
comments_freelance$text <- stringr::str_replace(comments_freelance$text, "front ", 
                                                "frontend")

# check I have done it right
comments_freelance %>% 
  filter(stringr::str_detect(text, regex(" front"))) %>% View()


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

words_by_id %>% select(starts_with("front"))

words_by_id %>% select(starts_with("back"))


#comments_who_is_hiring <- populate_comments(who_is_hiring_march_17_item$kids)


