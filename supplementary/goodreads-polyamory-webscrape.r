Sys.setenv(GOODREADS_KEY = readline("Enter Goodreads API key: "))


library(rgoodreads)
library(httr)

# apply goodreads site prefix
prefix <- function(x) str_c("https://www.goodreads.com", x)

# clear whitespace both around and within text
rm_ws <- function(x) {
  x %>%
    gsub(pattern = "[[:space:]]+", replace = " ") %>%
    gsub(pattern = "(^ )|( $)", replace = "")
}

library(tidyverse)
library(rvest)

# list of book lists
polyamory_listopia <- "/list/tag/polyamory" %>%
  prefix() %>%
  read_html() %>%
  html_nodes(".listTitle") %>%
  {tibble(list = html_text(.), link = html_attr(., "href"))}

# get book links
list_books <- function(list_link) {
  list_link %>%
    prefix() %>%
    read_html() %>%
    html_nodes(".bookTitle") %>%
    {tibble(title = html_text(.), link = html_attr(., "href"))} %>%
    mutate(title = rm_ws(title))
}

# get book details
book_info <- function(book_link) {
  # html
  book_html <- book_link %>%
    prefix() %>%
    read_html()
  
  # title
  book_title <- book_html %>%
    html_node("#bookTitle") %>%
    html_text() %>% rm_ws()
  # author(s)
  book_authors <- book_html %>%
    html_nodes(".authorName") %>%
    html_text() %>% rm_ws()
  # description text
  book_description <- book_html %>%
    html_node("#descriptionContainer") %>%
    html_text() %>% rm_ws()
  # genres
  book_genres <- book_html %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text() %>% rm_ws()
  
  # tibble
  tibble(
    title = book_title,
    authors = paste(book_authors, collapse = "|"),
    description = book_description,
    genres = paste(book_genres, collapse = "|")
  )
}

# tibbulate list books
polyamory_lists <- tibble()
n_lists <- nrow(polyamory_listopia)
pb <- progress_estimated(n_lists)
for (i in 1:n_lists) {
  pb$pause(1)$tick()$print()
  list_i <- polyamory_listopia$link[i] %>%
    list_books() %>%
    mutate(list = polyamory_listopia$list[i])
  polyamory_lists <- bind_rows(polyamory_lists, list_i)
}
polyamory_lists %>%
  group_by(title, link) %>%
  summarize(lists = paste(list, collapse = "|")) %>%
  print() -> polyamory_lists

# tibbulate book details
polyamory_books <- tibble()
n_books <- nrow(polyamory_lists)
pb <- progress_estimated(n_books)
for (i in 1:n_books) {
  pb$pause(1)$tick()$print()
  book_i <- polyamory_lists$link[i] %>%
    book_info()
  polyamory_books <- bind_rows(polyamory_books, book_i)
}

# combine list and book info
polyamory_booklist <- bind_cols(
  rename(polyamory_lists, title_ref = title),
  rename(polyamory_books, title_page = title)
)
write_rds(
  polyamory_booklist,
  here::here("supplementary/goodreads-polyamory-booklist.rds")
)
