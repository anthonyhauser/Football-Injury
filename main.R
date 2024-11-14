source("R/000_setup.R")

countries = c("England","France","Germany","Spain","Italy")#countries="England"
years=2020:2024
injury_df <- rbindlist(lapply(as.list(country),function(x) tm_league_injuries(country_name = x)))

injury_df %>% filter(grepl("Cruciate",injury)) %>% View()


hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")

#player squad, by season
team_players = list()
list_id=0
for(year in years){
  print(year)
  for(country in countries){
    print(country)
    list_id=list_id+1
    team_urls <- tm_league_team_urls(country_name = country, start_year = year)
    team_players[[list_id]] = tm_squad_stats(team_url = team_urls) %>% 
      dplyr::mutate(year=year)
  }
}
team_players_df=rbindlist(team_players)
saveRDS(team_players_df,"data/team_players_df_England.RDS")

team_players_df = rbindlist(team_players) %>% 
  dplyr::filter(minutes_played > 270)

saveRDS(d,"data/injury_history.RDS")
players_urls = team_players_df %>% pull(player_url) %>% unique()
d=tm_player_injury_history(player_urls = players_urls)
saveRDS(d,"data/injury_history.RDS")



d = rbindlist(team_players) %>% 
  dplyr::filter(minutes_played > 270)


d %>% group_by(team_name,country,year) %>% 
  dplyr::summarise(n=n()) %>% View()
d %>% 
  dplyr::mutate(duration=as.numeric(gsub(" days", "", duration))) %>% 
  filter(injury=="Cruciate ligament tear",!is.na(injured_until),duration<180)


d %>% filter(grepl("Cruciate",injury),!is.na(injured_until)) %>% 
  dplyr::mutate(duration=as.numeric(gsub(" days", "", duration))) %>% 
  group_by(injury) %>% 
  dplyr::summarise(n=n(),
                   min=min(duration),
                   max=max(duration),.groups="drop")


