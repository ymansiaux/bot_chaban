library(rtweet)
library(glue)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(emo)

# https://www.r-bloggers.com/2022/01/tips-for-building-a-twitter-bot-with-r-and-github-actions/?utm_source=phpList&utm_medium=email&utm_campaign=R-bloggers-daily&utm_content=HTML
# https://github.com/matt-dray/londonmapbot/blob/master/.github/workflows/londonmapbot.yml

botChaban_token <- create_token(
  app = "botChaban",
  consumer_key =    Sys.getenv("BOT_CHABAN_TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("BOT_CHABAN_TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("BOT_CHABAN_TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("BOT_CHABAN_TWITTER_ACCESS_TOKEN_SECRET")
)

# vigilance il faut resupprimer à chaque fois /home/y.mansiaux/.rtweet_token.rds

get_chaban_data <- fromJSON(
  glue("https://opendata.bordeaux-metropole.fr/api/records/1.0/search/?dataset=previsions_pont_chaban&q=date_passage+%3E%3D+%22{Sys.Date()-7}%22&rows=10000"
  )
)$records


chaban_data <- as_tibble(get_chaban_data) %>%
  unnest(cols = c(fields)) %>%
  mutate(date_fermeture = as_datetime(paste0(date_passage,"T",fermeture_a_la_circulation,":00")),
         date_reouverture = as_datetime(paste0(date_passage,"T",re_ouverture_a_la_circulation,":00"))
  ) %>%
  mutate(date_reouverture = case_when(
    date_reouverture < date_fermeture ~ add_with_rollback(date_reouverture, days(1)),
    TRUE ~ date_reouverture
  ))


datetime_bridge <- Sys.time()


# ouvert
# datetime_bridge <- as_datetime("2022-08-20T23:30:00")

# ferme
# datetime_bridge <- as_datetime("2022-08-28T23:30:00")

# ferme bientot
# datetime_bridge <- as_datetime("2022-08-28T22:30:00")

if(nrow(chaban_data) >0) {


  closed_bridge <- any(chaban_data$date_fermeture  <= datetime_bridge & chaban_data$date_reouverture > datetime_bridge)
  closing_today <- any(as_date(chaban_data$date_fermeture) == as_date(datetime_bridge))

  if(!closed_bridge & !closing_today) {
    message <- glue("Bonjour Twitter, le pont Chaban Delmas est actuellement ouvert {emo::ji('bridge')}.
                  Aucune fermeture n'est prévue aujourd'hui. {emo::ji('car')}{emo::ji('bike')}{emo::ji('walk')}")
  } else if(!closed_bridge & closing_today) {

    filtered_data <- chaban_data[(as_date(chaban_data$date_fermeture) == as_date(datetime_bridge)),]
    heure_fermeture <- filtered_data %>% pull(date_fermeture) %>% format(format = "%Hh%M")

    message <- glue("Bonjour Twitter, le pont Chaban Delmas est actuellement ouvert {emo::ji('bridge')}{emo::ji('car')}{emo::ji('bike')}{emo::ji('walk')}.
                  {emo::ji('warning')} Une fermeture est néanmois prévue à {heure_fermeture}")


  } else {
    filtered_data <- chaban_data[chaban_data$date_fermeture  <= datetime_bridge & chaban_data$date_reouverture > datetime_bridge,]

    reouverture <- filtered_data %>% pull(date_reouverture) %>% format(format = "%d %B %Hh%M")

    message <- glue("Bonjour Twitter, le pont Chaban Delmas est actuellement fermé {emo::ji('boat')}.
                  La réouverture est prévue pour le {reouverture} {emo::ji('bridge')}")


  }

} else {

  message("Les données d'ouverture / fermeture du pont Chaban Delmas ne sont pas accessibles ce jour {emo::ji('sad')}.")

}


rtweet::post_tweet(
  status = message,
  token = botChaban_token
)



