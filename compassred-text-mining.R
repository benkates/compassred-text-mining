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
for (folder in c(services,solutions,about,employees,careers,blog_posts)){
  
  #scrape page and extract html nodes with class "sqs-block-content" (commonly used on page for main content)
  if (!exists("html_body")){
  html_body <- html_nodes(getCR(folder),css=".sqs-block-content")
  }
  else{
    html_body <- html_body %>%
      c(html_nodes(getCR(folder),css=".sqs-block-content"))
  }
  
  Sys.sleep(1)
} 

#create output vector then iterate through nodes (avoid the ones we don't want)
text_vec <- character()
for (html_snippet in html_body){
  
  #check to make sure there's content
  if (length(html_nodes(html_snippet,"h1")) > 0 & length(html_nodes(html_snippet,xpath="//p|blockquote")) == 0){next}
  
  #check for footer (cheating a little bit)
  footer_text <- "CompassRed, Inc. | All rights reserved. © 2018 | wilmington, de | philadelphia, pa | "
  text <- html_text(html_snippet)
  if (substr(text, 0, nchar(footer_text)) == footer_text){next}
  
  #check for newsletter
  newsletter_text <- "Sign up for weekly data stories @ The Data Lab"
  text <- html_text(html_snippet)
  if (substr(text, 0, nchar(newsletter_text)) == footer_text){next}
  
  #check for nodes containing all whitespace
  if (grepl("\n\n\n",text)){next}
  
  #extract text from p nodes
  content_nodes <- html_nodes(html_snippet,xpath="p|blockquote")
  for (node in content_nodes){
    text_vec <- append(text_vec, html_text(node), after = length(text_vec))
    
  }
}


#TRANSFORMING####
#tokenize words
text_list <- as_tibble(text_vec)
text_tokenized <- text_list %>% unnest_tokens(word,value)

#take out stop words and count
data(stop_words)
text_tokenized %>% anti_join(stop_words) %>%
  group_by(word) %>% summarise(count=n()) %>%
  arrange(desc(count))
