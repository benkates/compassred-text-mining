#install.packages("rvest")
library(rvest)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidytext")
library(tidytext)

#define function to scrape http://compassred.com based on folder provided
getCR <- function(folder){
  return(read_html(paste0("http://compassred.com",folder)))
}

#EXTRACTING####
#go to homepage and grab links in navigation
hrefs_in_menu <- html_nodes(getCR(""),xpath="//nav//a") %>% html_attr('href') %>% unique() %>% na.omit()
services <- hrefs_in_menu[1:5]
solutions <- hrefs_in_menu[6]
about <- hrefs_in_menu[7]
employees_master <- "/employee-bios" #cheating here since this is more of a master link
careers <- hrefs_in_menu[9]
blog <- hrefs_in_menu[11]
#excluding contact and client login pages

#grab individual employee links
employees <- html_nodes(getCR(employees_master),xpath="//a[contains(@href,'employee-bios/')]") %>%
  html_attr('href') %>% unique()

#grab blog pages showing older blog posts
bool <- F
blog_start_page <- html_node(getCR(blog),xpath="//a[contains(@href,'/blog?offset=') and not(contains(@href,'reversePaginate'))]") %>% html_attr('href')
blog_older_pages <- c(blog_start_page)
while(bool == F){
  blog_current_page <- tail(blog_older_pages,n=1)
  blog_node <- html_node(getCR(blog_current_page),xpath="//a[contains(@href,'/blog?offset=') and not(contains(@href,'reversePaginate'))]")
  if(length(blog_node) > 0){
    blog_older_pages <- blog_older_pages %>% c(blog_node %>% html_attr('href'))
  }
  else{
    bool <- T
  }
}

#grab blog post links from "Read more" text
blog_posts <- c()
for (blog_post in c(blog,blog_older_pages)){
  blog_posts <- blog_posts %>%
    c(html_nodes(getCR(blog_post),xpath="//a[contains(@href,'/blog/') and contains(text(),'Read more')]")
      %>% html_attr('href'))
}

#scrape content
scrape_df <- tibble(node = character(),folder = character())

for (folder in c(services,solutions,about,employees,careers,blog_posts)){
  
  #scrape page and extract html nodes with class "sqs-block-content" (commonly used on page for main content)
  html_body <- html_text(html_nodes(getCR(folder),".sqs-block-content"))
  scrape_df <- scrape_df %>% add_row(node=html_body,folder=folder)
  
  Sys.sleep(1) #don't overwhelm the site
} 

#remove text that looks like the footer
footer_text <- "CompassRed, Inc. \\| All rights reserved"
scrape_df <- scrape_df[!(grepl(footer_text,scrape_df$node)),]

#TRANSFORMING####
#tokenize words
text_tokenized <- scrape_df %>% unnest_tokens(word,node)

#take out stop words and count
data(stop_words)
text_df <- text_tokenized %>% anti_join(stop_words) %>%
  group_by(folder,word) %>% summarise(count=n()) %>%
  arrange(desc(count))





