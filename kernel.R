#### prep environment ####
setwd("../input")
library(tidyverse)
library(data.table)
#### import the csv files ####
# data.table::fread is fast so i'm using that
csv_files <- list.files(pattern = "csv$")
for (i in csv_files) {
  print(paste0("importing ", i, "..."))
  element_name <- gsub(".csv", "", i, fixed = TRUE)
  assign(element_name, data.table::fread(i), envir = .GlobalEnv)
  rm(i, element_name)
}
#### find the concussion plays ####
# i think playids can be recycled across seasons, but gamekeys can't
# could be wrong! comment if that's the case
injury_gamekey_playids <- paste0(`video_footage-injury`$gamekey, "_", `video_footage-injury`$playid)
control_gamekey_playids <- paste0(`video_footage-control`$gamekey, "_", `video_footage-control`$playid)
gamekey_playids <- unique(c(injury_gamekey_playids, control_gamekey_playids))
# loop through the objects in environment with next gen stats data, extract relevant plays
# not sure if necessary; comment if this is all unnecessary
ngs_files <- ls(pattern = "^NGS")
ngs_data <- list()
for (i in ngs_files) {
  print(paste0("inspecting ", i, "..."))
  temp <- get(i)
  temp <- temp[, gamekey_playid := paste0(GameKey, "_", PlayID)]
  temp <- temp[gamekey_playid %in% gamekey_playids]
  if (nrow(temp) == 0) {
    next
  }
  ngs_data[[i]] <- temp
  rm(temp, i)
}
ngs_dt <- data.table::rbindlist(ngs_data)
# tidy up environment to free up memory if necessary
rm(ngs_data)
rm(list = ls(pattern = "^NGS"))
#### enrich concussion plays ####
# add game_data
ngs_dt <- game_data[ngs_dt, on = "GameKey"]
# add player_punt_data
position_lu <- player_punt_data[, list(pos = Position[1]), by = GSISID]
ngs_dt <- position_lu[ngs_dt, on = "GSISID"]
# add play_information
play_information <- play_information[, gamekey_playid := paste0(GameKey, "_", PlayID)]
ngs_dt <- play_information[ngs_dt, on = "gamekey_playid"]
# add play_player_role_data
play_player_role_data <- play_player_role_data[, gamekey_playid_gsisid := paste0(GameKey, "_", PlayID, "_", GSISID)]
ngs_dt <- ngs_dt[, gamekey_playid_gsisid := paste0(gamekey_playid, "_", GSISID)]
ngs_dt <- play_player_role_data[ngs_dt, on = "gamekey_playid_gsisid"]
# add concussion play info later, but add the identifier now
concussion_play_data <- video_review[, gamekey_playid := paste0(GameKey, "_", PlayID)]
# tidy up columns
cols_to_remove <- names(ngs_dt)[grepl("^i|[12]$", names(ngs_dt))]
ngs_dt <- ngs_dt[, !cols_to_remove, with = FALSE]
# fractional seconds are jerks in R; 
# parse manually and get something numeric;
# if there's a better way please comment!
parseable_time <- data.table::tstrsplit(ngs_dt$Time, split = " ")
date_part <- as.Date(parseable_time[[1]])
num_date_part <- as.numeric(date_part)
time_part <- parseable_time[[2]]
split_time_part <- data.table::tstrsplit(time_part, split = ":")
num_time_part <- 
  (as.numeric(split_time_part[[1]]) * 360) + 
  (as.numeric(split_time_part[[2]]) * 60) + 
  as.numeric(split_time_part[[3]])
num_datetime <- num_date_part + num_time_part
ngs_dt$ordinal_time <- num_datetime
# make a table for the roles to identify which side of the ball they're on
role_lu <- ifelse(
  grepl("^G[LR]|P[LR]W|P[LR]T|P[LR]G|^PC|^PLS|PP[LR]|^P$", ngs_dt$Role), 
  "punt", "rec")
ngs_dt <- ngs_dt[, Side := role_lu]
#### make animations ####
# initially i filtered to include only frames between start and end events, but currently include everything
# play_start_events <- c("line_set", "ball_snap")
# play_end_events <- c("tackle", "punt_downed", "fumble_offense_recovered",
#                      "out_of_bounds", "fair_catch", "touchdown")
# loop through the gamekey_playids, generate animations
animations <- list()
for (i in gamekey_playids) {
  print(paste0("animating gamekey_playid ", i))
  selected_play <- ngs_dt[gamekey_playid == i]
  # get info about the game for title
  matchup <- selected_play$Home_Team_Visit_Team[[1]]
  gamedate <- selected_play$Game_Date[[1]]
  punting_team <- selected_play$Poss_Team[[1]]
  receiving_team <- ifelse(selected_play$HomeTeamCode[[1]] == punting_team,
                           selected_play$VisitTeamCode[[1]],
                           selected_play$HomeTeamCode[[1]])
  # get time bounds for filtering
  lower_time_bound <- 0 # min(selected_play[Event %in% play_start_events, ordinal_time])
  upper_time_bound <- Inf # max(c(lower_time_bound + 20, max(selected_play[Event %in% play_end_events, ordinal_time])))
  filtered_play <- selected_play[ordinal_time >= lower_time_bound & ordinal_time <= upper_time_bound]
  if (nrow(filtered_play) == 0) {
    next
  }
  # make a the input for the plot
  plot_input <- filtered_play %>%
    tibble::as.tibble() %>%
    dplyr::left_join(concussion_play_data, by = "gamekey_playid", suffix = c("", "_cd")) %>%
    dplyr::mutate(color_id = dplyr::if_else(GSISID == GSISID_cd, "concussed_player",
                                            dplyr::if_else(GSISID == Primary_Partner_GSISID, "concussion_partner",
                                                           Side))) %>%
    dplyr::mutate(color_id = dplyr::if_else(is.na(color_id), Side, color_id)) %>%
    dplyr::mutate(display_time = ordinal_time - min(ordinal_time)) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    na.omit()
  p <- ggplot2::ggplot(plot_input, ggplot2::aes(x = x,
                                                y = y, 
                                                color = color_id,
                                                ids = GSISID,
                                                label = pos,
                                                frame = display_time)) +
    ggplot2::coord_cartesian(xlim = c(0, 120), ylim = c(-10, 53)) +
    ggplot2::geom_point(size = 3, alpha = .6) +
    ggplot2::geom_text(ggplot2::aes(label = pos), size = 1.667, color = 'black') +
    ggplot2::geom_text(ggplot2::aes(x = 10, y = 50, label = Event), color = 'darkgrey') +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("concussed_player" = "firebrick1",
                                           "concussion_partner" = "goldenrod2", 
                                           "rec" = "grey50", 
                                           "punt" = "grey80")) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor.y = ggplot2::element_blank()) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::scale_x_continuous(breaks = seq(0, 120, 10), 
                                labels = c('', 'G', '10', '20', '30', '40', '50', '40', '30', '20', '10', 'G', '')) +
    ggplot2::scale_y_continuous(breaks = c(0, 53.3), 
                                labels = c('', ''))
  animated_p <- plotly::ggplotly(p) %>%
    plotly::layout(margin = list(t = 50),
                   title = paste0(matchup, ", ", gamedate, " (", punting_team, " punting to ", receiving_team, ")")) %>%
    plotly::animation_opts(frame = 100)
  animations[[i]] <- animated_p
  # save output
  ## not run
  # new_fn <- paste0("gamekey_playid_", i, ".html")
  # htmlwidgets::saveWidget(plotly::as_widget(animated_p), new_fn)
}
# take a look at allllllll the plays
## not run
# animations
