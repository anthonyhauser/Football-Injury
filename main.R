source("R/000_setup.R")

countries = c("England","France","Germany","Spain","Italy") #countries="England"
years=2000:2024

#player squad, by season
for(country in countries){
  print(country)
  team_players = list()
  list_id=0
  for(year in years){
    print(year)
    list_id=list_id+1
    team_urls <- tm_league_team_urls(country_name = country, start_year = year)
    team_players[[list_id]] = tm_squad_stats(team_url = team_urls) %>% 
      dplyr::mutate(year=year)
  }
  team_players_df=rbindlist(team_players)
  saveRDS(team_players_df,paste0("data/team_players_df_",country,".RDS"))
}

#Injuries
team_players_df = rbindlist(lapply(as.list(countries),function(x) readRDS(paste0("data/team_players_df_",x,".RDS"))))
players_urls = team_players_df %>% pull(player_url) %>% unique()
chunk_size <- 100
chunks <- split(1:length(players_urls), ceiling(seq_along(1:length(players_urls)) / chunk_size))
names(chunks) = paste0("part",1:length(chunks))
for(i in 1:length(chunks)){
  print(i)
  player_injuries_df = tm_player_injury_history(player_urls = players_urls[chunks[[i]]])
  saveRDS(player_injuries_df,paste0("data/player_injuries_df_",names(chunks)[i],".RDS"))
}
#load data
player_injuries_df = readRDS("data/injury_history.RDS")
player_injuries_df2 = rbindlist(team_players) %>% 
  dplyr::filter(minutes_played > 270)

#ACL injuries
player_injuries_df %>% filter(grepl("Cruciate",injury)) %>% pull(injury) %>% unique()
#number of ACL injuries
player_injuries_df2 %>%
  group_by(team_name,country,year) %>% 
  dplyr::summarise(n=n())
#Recovery duration, check for ACL injuries
player_injuries_df2 %>%
  filter(grepl("Cruciate",injury),!is.na(injured_until)) %>% 
  dplyr::mutate(duration=as.numeric(gsub(" days", "", duration))) %>% 
  group_by(injury) %>% 
  dplyr::summarise(n=n(),
                   min=min(duration),
                   max=max(duration),.groups="drop")
player_injuries_df2 %>% 
  dplyr::mutate(duration=as.numeric(gsub(" days", "", duration))) %>% 
  filter(injury=="Cruciate ligament tear",!is.na(injured_until),duration<180)



