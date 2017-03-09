require(httr)

require(tidyr)
require(dplyr)

require(tidytext)

HN_API_url <- "https://hacker-news.firebaseio.com/v0/"

freelance_march_17 <- "13764729"
who_is_hiring_march_17 <- "13764728"

get_item_url <- function(id){
  paste(HN_API_url, "item/", id, ".json", sep = "")
}

freelance_march_17_item <- httr::content( httr::GET( get_item_url(freelance_march_17) ) )
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

comments_freelance <- populate_comments(freelance_march_17_item$kids)
comments_who_is_hiring <- populate_comments(who_is_hiring_march_17_item$kids)
hiring <- tibble::tibble()


word_count <- comments %>%
  tidytext::unnest_tokens(words, text) %>%
  dplyr::select(id, words) %>%
  dplyr::group_by(id, words) %>%
  summarise(n = n())

total_words <- word_count %>%
  dplyr::ungroup() %>%
  dplyr::distinct(words)

words_by_id <- tidyr::spread(word_count, words, n)
