# settings ####

library(tidyverse)
library(shinythemes)
library(janitor)
library(shiny)
library(ggraph)
library(igraph)

setwd("C:/Projects/reproquest/reproquest_results")

prepare_survey_coffee <- function(survey_csv, skinny = TRUE) {
  survey <- readr::read_csv(survey_csv, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::select(-email, -browser, -os, -referrer) %>%
    dplyr::rename(
      date = date_taken_europe_zurich,
      time = time_taken_europe_zurich,
      nickname = q1_choose_a_nickname,
      
      # caution needed q number from survey does NOT match Q number in data frame
      # remain vigilant in case any changes are needed
      # or if any questions were added or removed in the survey
      Question1 = dplyr::starts_with("q2"),
      Question2 = dplyr::starts_with("q3"),
      Question3 = dplyr::starts_with("q4"),
      Question4 = dplyr::starts_with("q5"),
      Question5 = dplyr::starts_with("q6"),
      Question6 = dplyr::starts_with("q7"),
      
    )
  
  # get answer letter in caps
  columns_to_transform <- survey %>%
    dplyr::select(dplyr::starts_with("Question")) %>%
    colnames()
  
  survey <- survey %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_transform),
        ~ stringr::str_to_upper(stringr::str_extract(.x, "^[A-Z]"))
      )
    )
  
  # get only what's needed for preparation further
  if (skinny == TRUE) {
    survey <- survey %>%
      dplyr::select(
        nickname,
        dplyr::starts_with("Question")
      )
  }
  
  return(survey)
}


# dendrogram ####
plt_dendro <- function(df) { # df will be time_data()
  d_dendr <- df %>%
    dplyr::select(
      nickname,
      dplyr::starts_with("Question")
    )
  
  # duplicated nicknames ####
  ## check for duplicated nicknames and modify them adding "_" and a consecutive number to each of them
  
  # gives you a data frame with a list of ids and the number of times they occurred
  n_occur <- data.frame(table(d_dendr$nickname))
  
  dupl_name <- n_occur[n_occur$Freq > 1, ]
  # tells you which ids occurred more than once.
  
  for (i in 1:nrow(dupl_name)) {
    row_id <- which(d_dendr$nickname == dupl_name$Var1[i])
    
    create_unique <- as.character(dupl_name[i, "Var1"])
    create_unique <- paste(create_unique, c(1:length(row_id)), sep = "_")
    
    d_dendr$nickname[row_id] <- create_unique
  }
  
  #####
  
  
  d_dendr <- d_dendr %>% arrange(Question1, Question2, Question3, Question4, Question5, Question6)
  
  # testing multiple equal answers combination ####
  # d_dendr[17,8] <- "D"
  # d_dendr[6,6] <- "B"
  # d_dendr[6,7] <- "D"
  # d_dendr[6,8] <- "A"
  
  # modify the single answer with the sequence of answers
  for (i in 3:ncol(d_dendr)) {
    d_dendr[[i]] <- paste(d_dendr[[i - 1]], d_dendr[[i]], sep = "_")
  }
  
  # without using a list and adding nickname immediately
  # creating a long df with one column "from" and one column "to"
  d1 <- data.frame(
    from = "origin",
    to = unique(d_dendr[[2]]),
    nickname = NA
  )
  
  q2 <- data.frame(
    from = d_dendr[[2]],
    to = d_dendr[[3]],
    nickname = d_dendr$nickname
  )
  
  q3 <- rbind(d1, q2)
  for (i in 4:ncol(d_dendr)) {
    d_temp <- data.frame(
      from = d_dendr[[i - 1]],
      to = d_dendr[[i]],
      nickname = d_dendr$nickname
    )
    q3 <- rbind(q3, d_temp)
  }
  
  # create vertices df
  vertices2 <- d_dendr %>%
    dplyr::select(-nickname) %>%
    tidyr::pivot_longer(dplyr::everything(), cols_vary = "slowest") %>%
    group_by(name) %>%
    dplyr::count(value) %>%
    ungroup()
  
  vertices2 <- vertices2 %>%
    dplyr::select(-name) %>%
    tibble::add_column(group = paste("group", seq(1:(nrow(vertices2))))) %>%
    dplyr::add_row(value = "origin", n = nrow(df), group = NA, .before = 1) %>%
    dplyr::rename(name = value, value = n)
  
  # create leaves names
  # 1. check if there are duplicated sequence of answers
  
  # TODO change LAST question number
  # last_col_name <- colnames(d_dendr)[ncol(d_dendr)]
  # leaves_name <- d_dendr[,c("nickname",last_col_name)] %>% dplyr::rename(to=as.character(last_col_name))
  # leaves_count <- d_dendr %>% dplyr::count(.data[[last_col_name]]) %>% dplyr::rename(to=as.character(last_col_name))
  
  leaves_name <- d_dendr[, c("nickname", "Question6")] %>% dplyr::rename(to = Question6)
  
  leaves_count <- d_dendr %>%
    count(Question6) %>%
    dplyr::rename(to = Question6)
  end_points <- nrow(leaves_count)
  
  if (sum(leaves_count$n > 1) == 0) {
    # recording the nickname to plot at the end of the dendrogram
    vertices2 <- merge(vertices2, leaves_name, by.x = "name", by.y = "to", all.x = T)
    vertices2$nickname[is.na(vertices2$nickname)] <- vertices2$group[is.na(vertices2$nickname)]
  } else {
    # create leaves names
    # leaves_name <- d_dendr %>% dplyr::count(.data[[last_col_name]]) %>% dplyr::rename(to=as.character(last_col_name))
    # 1. check if there are duplicated sequence of answers
    # leaves_count <- d_dendr %>% count(Question7) %>% dplyr::rename(to=as.character(last_col_name))
    
    # create leaves names
    leaves_name <- d_dendr[, c("nickname", "Question6")] %>% dplyr::rename(to = Question6)
    # 1. check if there are duplicated sequence of answers
    leaves_count <- d_dendr %>%
      count(Question6) %>%
      dplyr::rename(to = Question6)
    
    dupl_answ <- leaves_count[leaves_count$n > 1, ]
    duplicated_df <- NULL
    
    for (i in 1:nrow(dupl_answ)) { # paste nicknames of each duplictaed answer sequence
      select_duplicates <- d_dendr[which(d_dendr$Question6 == dupl_answ$to[i]), c("nickname", "Question6")]
      
      
      dupl_nickname <- data.frame(
        nickname = paste(select_duplicates$nickname, collapse = "-"),
        to = select_duplicates$Question6[1]
      )
      
      duplicated_df <- rbind(duplicated_df, dupl_nickname)
    }
    
    # remove duplicated leaves
    leaves_count1 <- leaves_count[-which(leaves_count$n > 1), ]
    leaves_count1 <- merge(leaves_count1, leaves_name)
    
    # create df for duplicated leaves with nickname
    leaves_count2 <- as.data.frame(leaves_count[which(leaves_count$n > 1), ])
    leaves_count2 <- merge(leaves_count2, duplicated_df, by = "to")
    
    leaves_count_final <- rbind(leaves_count1, leaves_count2)
    vertices2 <- merge(vertices2, leaves_count_final[, c("to", "nickname")], by.x = "name", by.y = "to", all.x = T)
    vertices2$nickname[is.na(vertices2$nickname)] <- vertices2$group[is.na(vertices2$nickname)]
  }
  
  mygraph <- graph_from_data_frame(q3, vertices = vertices2)
  
  p <- ggraph(mygraph, layout = "dendrogram", height = ) +
    geom_edge_diagonal(width = 2) +
    
    geom_node_text(
      aes(
        label = c("origin", str_sub(name[2:length(name)], -1)),
        color = group, size = 20
      ),
      angle = 45, hjust = 1, nudge_x = -end_points * .01
    ) +
    
    geom_node_text(
      aes(
        label = value,
        color = group, size = 20
      ),
      angle = 45, hjust = 1, nudge_x = end_points * .01
    ) +
    
    
    geom_node_text(aes(filter = leaf, label = nickname, color = nickname, size = 20), angle = 45, hjust = 1, nudge_y = -.5) +
    geom_node_point(aes(color = group), alpha = 0.6) +
    geom_node_point(aes(size = value, color = group), alpha = 0.6) +
    ylim(-3, NA) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme(legend.position = "none") #+
  
  p
}




####################################################################################
d <- prepare_survey_coffee("data/coffee_survey1.csv", skinny = FALSE)
d$date <- mdy(d$date)

d <- d %>% filter(date > "2024-10-08")

# check if there are duplicated names and fix it
n_occur <- data.frame(table(d$nickname))
dupl_name <- n_occur[n_occur$Freq > 1,]

for (i in 1:nrow(dupl_name)) {
  row_id <- which(d$nickname == dupl_name$Var1[i])
  create_unique <- as.character(dupl_name[i,"Var1"])
  create_unique <- paste(create_unique, c(1:length(row_id)), sep="_")
  
  d$nickname[row_id] <- create_unique
}


plt_dendro(d)
ggsave("data/plot.png")
