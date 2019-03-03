library(rvest)
library(tidyverse)
library(tidytext)

#EXTRACTING####
#go to homepage and grab links in navigation
webpage <- read_html("http://compassred.com")
hrefs_in_menu <- html_nodes(webpage,xpath="//nav//a") %>% html_attr('href') %>% unique() %>% na.omit()
services <- hrefs_in_menu[1:5]
# solutions <- hrefs_in_menu[6]
# about <- hrefs_in_menu[7]
# employees <- "/employee-bios" #cheating here since this is more of a master link
# careers <- hrefs_in_menu[9]
# contact <- hrefs_in_menu[10]
# blog <- hrefs_in_menu[11]
# # excluding client login page

#scrape text from services_urls
text_vec <- character()
for (folder in services){
  #scrape page and extract html nodes with class "sqs-block-content" (commonly used on page for main content)
  url <- paste0("http://compassred.com",folder)
  webpage <- read_html(url)
  html_body <- html_nodes(webpage,css=".sqs-block-content")
  
  #create output vector then iterate through nodes (avoid the ones we don't want)
  for (html_snippet in html_body){
    #check to make sure there's content
    if (length(html_nodes(html_snippet,"h1")) > 0 & length(html_nodes(html_snippet,xpath="//p|blockquote")) == 0){next}
    #check for footer (cheating a little bit)
    footer_text <- "CompassRed, Inc. | All rights reserved. © 2018 | wilmington, de | philadelphia, pa | "
    text <- html_text(html_snippet)
    if (substr(text, 0, nchar(footer_text)) == footer_text){next}
    #check for nodes containing all whitespace
    if (grepl("\n\n\n",text)){next}
  
    #extract text from p nodes
    content_nodes <- html_nodes(html_snippet,xpath="p|blockquote")
    for (node in content_nodes){
      text_vec <- append(text_vec, html_text(node), after = length(text_vec))
    }
    
    Sys.sleep(1) #don't overwhelm the site
  }
}

#TRANSFORMING####
#tokenize words
text_list <- as_tibble(text_vec)
text_tokenized <- text_list %>% unnest_tokens(word,value)

#take out stop words and count
data(stop_words)
text_tokenized %>% anti_join(stop_words) %>% group_by(word) %>% summarise(count=n()) %>% arrange(desc(count))



