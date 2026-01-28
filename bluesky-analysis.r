library(readr)

posts <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(posts)

library(dplyr)

glimpse(posts)

followers <- read_csv("INSERT_FILE_NAME_HERE")

names(followers)

glimpse(followers)

following <- read_csv("INSERT_FILE_NAME_HERE")

names(following)

glimpse(following)

library(quanteda)

bluesky_corpus <- posts %>% 
  distinct(uri, .keep_all = TRUE) %>% 
  select(id, uri, cid,
         author_handle, author_name,
         indexed_at, reply_count, repost_count,
         like_count, quote_count,
         is_reskeet,
         text) %>% 
  corpus(docid_field = "uri",
         text_field = "text")

tokens_bluesky <- tokens(bluesky_corpus,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       remove_url = TRUE)

tokens_bluesky <- tokens_remove(tokens_bluesky,
                              stopwords("de"))

tokens_bluesky

dfm_bluesky <- dfm(tokens_bluesky)

library(quanteda.textstats)

dfm_bluesky %>%
  dfm_remove(pattern = c("bsky.social", "dass", # zusätzliche Entfernung von "custom stop words"
                         "@*", "#*")) %>% # ohne User Mentions und Hashtags
  textstat_frequency(n = 20)

dfm_tag <- dfm_select(dfm_bluesky, pattern = "#*")
toptag <- names(topfeatures(dfm_tag, 50)) # 50 häufigste Hashtags
head(toptag, 10) # 10 häufigste Hashtags

dfm_users <- dfm_select(dfm_bluesky, pattern = "@*")
topuser <- names(topfeatures(dfm_users, 50)) # 50 häufigste User Mentions
head(topuser, 10) # 10 häufigste User Mentions

library(quanteda.textplots)

dfm_bluesky %>% 
  dfm_remove(pattern = c("bsky.social", "dass", # zusätzliche Entfernung von "custom stop words"
                         "@*", "#*")) %>% # ohne User Mentions und Hashtags
  dfm_trim(min_termfreq = 20) %>%
  textplot_wordcloud()

library(ggplot2)

tstat_freq <- dfm_bluesky %>% 
  dfm_remove(pattern = c("bsky.social", "dass", # zusätzliche Entfernung von "custom stop words"
                         "@*", "#*")) %>%  # ohne User Mentions und Hashtags
  textstat_frequency(n = 20)

ggplot(tstat_freq, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col() + 
  labs(x = "Frequency", y = "Feature") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

posts %>% 
    count(author_handle) %>% 
    arrange(desc(n)) %>% 
    head(10)

tstat_key <- dfm_bluesky %>% 
  dfm_remove(pattern = c("bsky.social", "dass", # zusätzliche Entfernung von "custom stop words"
                         "@*", "#*")) %>%  # ohne User Mentions und Hashtags
  textstat_keyness(target = dfm_bluesky$author_handle == "insert_user_handle") # hier den Namen des zu vergleichenden Accounts einfügen

textplot_keyness(tstat_key)

library(igraph)

reposts <- posts %>%
  filter(is_reskeet == TRUE) %>%
  select(source = id, target = author_handle) %>% 
  count(source, target, name = "weight") %>% 
  filter(source != target) %>%
  filter(weight >= 2)

head(reposts)

g1 <- repost_network <- graph_from_data_frame(reposts,
                                          directed = TRUE)

plot(g1, 
     edge.width = E(g1)$weight * 2, # dickere Edges für größere Gewichte (mehr Reposts)
     edge.arrow.size = 0.5,
     edge.label = E(g1)$weight,
     main = "Weighted Repost Network")

follower_net <- followers %>%
  select(source = actor_handle, target = id)

g2 <- follower_network <- graph_from_data_frame(follower_net,
                                              directed = TRUE)

g2

following_net <- following %>%
  select(source = id, target = actor_handle)

g3 <- following_network <- graph_from_data_frame(following_net,
                                                 directed = TRUE)

g3
