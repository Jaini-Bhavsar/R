library(rvest)
library(dplyr)

get_doi = function(article_link) {
  ar_page = read_html(article_link)
  cast = ar_page %>% html_nodes(".c-bibliographic-information__list-item--doi .c-bibliographic-information__value") %>%
    html_text() %>% paste(collapse = ",")
  return(cast)
}

get_keyword = function(article_link) {
  ar_page1 = read_html(article_link)
  cast1 = ar_page1 %>% html_nodes(".c-article-subject-list") %>%
    html_text() %>% paste(collapse = ",")
  return(cast1)
}

get_fulltext = function(article_link) {
  ar_page2 = read_html(article_link)
  cast2 = ar_page2 %>% html_nodes(" #Sec1-content p , #Abs1-content p") %>%
    html_text() %>% paste(collapse = ",")
  return(cast2)
}


art_c = data.frame()
page_result=10
if (page_result %in% seq(1, 13, by=1)){
  link = paste0("https://mobilednajournal.biomedcentral.com/articles?query=&volume=", 
                page_result, "&searchType=&tab=keyword")
  page = read_html(link)
  title = page %>% html_nodes(".c-listing__title a") %>% html_text()
  article_link = page %>% html_nodes(".c-listing__title a") %>%
    html_attr("href") %>% paste("https://mobilednajournal.biomedcentral.com", ., sep="")
  year = page %>% html_nodes(".c-meta__item+ .c-meta__item") %>% html_text()
  authorName=page %>% html_nodes(".u-mb-0") %>% html_text()
  abstract=page %>% html_nodes("p.c-listing__text") %>% html_text()
  doi = sapply(article_link, FUN = get_doi, USE.NAMES = FALSE)
  keyword=sapply(article_link, FUN = get_keyword, USE.NAMES = FALSE)
  fulltext=sapply(article_link, FUN = get_fulltext, USE.NAMES = FALSE)
  art_c = rbind(art_c, data.frame(title, year,authorName,abstract,doi,keyword,fulltext, stringsAsFactors = FALSE))
  print(paste("Page:", page_result))  
}

write.csv(art_c,"/Users/jainibhavsar/Desktop/Result.csv")
