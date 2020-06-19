get_info_from_opened_tab <- function(chrome, save_dir){
  qpath <- sprintf("%s/session/%s/window", chrome$serverURL, chrome$sessionInfo[["id"]])
  chrome$queryRD(qpath, "POST", qdata = list(handle = chrome$getWindowHandles()[[1]]))
  
  url <- chrome$getCurrentUrl()[[1]]
  link_id <- meta_data %>%
    filter(link == url) %>%
    pull(id)
  details <- get_all_details("") %>%
    glimpse
  if(inherits(details, "try-error")){
    test <- chrome %>% 
      get_real_source_code() %>%
      check_captcha
    return(tibble())
  }
  write_rds(details, paste0(save_dir, "/", link_id, ".rds"))
  chrome %>% close_tab()
}


new_tab <- function(chrome){
  chrome %>%
    doc_key_down("ctrl") %>%
    selinput::doc_type("t") %>%
    doc_key_up("ctrl")
}

close_tab <- function(chrome){
  chrome %>%
    doc_key_down("ctrl") %>%
    selinput::doc_type("w") %>%
    doc_key_up("ctrl")
}


check_captcha <- function(page){
  trig <- page %>%
    html_nodes(".g-recaptcha-bubble-arrow")  %>%
    length %>%
    magrittr::is_greater_than(0)
  if(trig){stop("Captcha required")}else{return(page)}
}

get_all_details <- function(link){
  
  # chrome %>% go(link)
  
  ft <- chrome %>% 
    get_real_source_code() %>%
    check_captcha %>%
    html_node("#readableContent") %>%
    html_text()
  
  elem <- suppressMessages(try(chrome %>% elements(".tab"), silent = T))
  if(!inherits(elem, "try-error") & length(elem) != 0){
    elem[[length(elem)]] %>% click()
  }
  
  details <- chrome %>% 
    get_real_source_code() %>%
    parse_details %>%
    mutate(ft = ft)
  
  
}


parse_details <- function(page){
  page %>%
    html_nodes(".display_record_indexing_row") %>% #bashR::simule_map(1)
    map_dfc(~{
      tmp <- .x %>%
        html_children() %>%
        map(html_text) 
      
      set_names(tibble(x= tmp[[2]]), tmp[[1]])
    }) 
}


parse_page <- function(page){
  # a <- page %>%
  page %>%
    html_nodes(".item") %>%
    map(html_node, ".resultHeader") %>%
    map(tidyweb::tidy_element, 3) %>% 
    imap_dfr(~{
      # print(.y)
      if(!"href" %in% names(.x)){return(tibble())}
      
      .x %>%
        filter(class == "titleAuthorETC" | str_detect(id, "result-header") | str_detect(href, "http")) %>%
        transmute(content = ifelse(is.na(href), text, href),
                  name = case_when(
                    str_detect(href, "http") ~ "link", 
                    str_detect(id, "result-header") ~ "title",
                    str_detect(text, "\\d{4}") ~ "meta",
                    class == "titleAuthorETC" ~ "author", 
                    T ~ "other"
                  )) %>%
      pivot_wider(names_from = name, values_from = content) %>%
      mutate_all(as.character)
    }) 
}