library(tidyverse)
# library(sentimentr)

# load sentimentr library manually, if needed
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sentimentr")
source("load_sentimentr_library.R")
setwd(current_wd)

# https://github.com/trinker/sentimentr

# get sentiment

# create mytext
mytext <- c(
        "I am not afraid of you",
        NA,
        "",
        "I love it [not really]", 
        "I'm not angry with you", 
        "I hate it when you lie to me.  It's so humiliating",
        "I'm not happpy anymore.  It's time to end it",
        "She's a darn good friend to me",
        "I went to the terrible store",
        "There is hate and love in each of us",
        "I'm no longer angry!  I'm really experiencing peace but not true joy.",
        
        str_c("Out of the night that covers me, Black as the Pit from pole to", 
              "pole, I thank whatever gods may be For my unconquerable soul.",
              "In the fell clutch of circumstance I have not winced nor cried",
              "aloud. Under the bludgeonings of chance My head is bloody, but unbowed.",
              "Beyond this place of wrath and tears Looms but the Horror of the", 
              "shade, And yet the menace of the years Finds, and shall find, me unafraid.",
              "It matters not how strait the gate, How charged with punishments", 
              "the scroll, I am the master of my fate: I am the captain of my soul."
        )    
        
)

# break mytext into sentences
mytext_sentences <- get_sentences(mytext)
mytext_sentences
mytext_sentences %>% class()

# get sentiment of mytext_sentences
mytext_sentiment <- sentiment(text.var = mytext_sentences)
mytext_sentiment


############################


# can also use a tbl
mytext_tbl <- tibble(text = mytext) %>% mutate(sentences = get_sentences(text))
mytext_tbl
mytext_tbl %>% unnest(sentences)

# get sentiment of mytext_tbl, which is scored at the sentence level; 
# github page has a good intuitive description of the scoring equation, including performance benchmarks
# basically it's positive/negative/emotion dictionary lookups, but also accounts for valence shifters for localized negation, amplication, de-amplication, etc
# note the exposition operator %$%, which exposes all the variables, but doesn't pass the df/tbl; this is used when the next funciton doesn't take data arg
mytext_sentiment <- mytext_tbl %$% sentiment(text.var = sentences)
mytext_sentiment

# extract_sentiment_terms
# note that since mytext_tbl_sentiment_terms has negative/positive variables as lists, it doesn't play well with tibble
# the convert_terms_to_tbl function below returns a tidy tbl
mytext_sentiment_terms <- mytext_tbl %$% extract_sentiment_terms(sentences)
mytext_sentiment_terms
mytext_sentiment_terms %>% glimpse()


##################################################################################################################################


# create convert_terms_to_tbl()
convert_terms_to_tbl <- function(terms_df) {
        
        # handle sentiment terms
        if(sum(c("negative", "neutral", "positive") %in% names(terms_df)) > 0) {
                
                # get tidy_negative_col
                tidy_negative_col <- terms_df %>% select(element_id, sentence_id, negative) %>% unnest(negative) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "negative") %>% rename(term = negative)
                
                # get tidy_neutral_col
                tidy_neutral_col <- terms_df %>% select(element_id, sentence_id, neutral) %>% unnest(neutral) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "neutral") %>% rename(term = neutral)
                
                # get tidy_positive_col
                tidy_positive_col <- terms_df %>% select(element_id, sentence_id, positive) %>% unnest(positive) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(sentiment = "positive") %>% rename(term = positive)
                
                # combine tidy_negative/neutral/positive_col
                tidy_terms_tbl <- bind_rows(tidy_negative_col, tidy_neutral_col, tidy_positive_col) %>%
                        arrange(element_id, sentence_id, sentiment)
                
                return(tidy_terms_tbl)
        }
        
        # handle emotion terms
        if(sum(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust") %in% names(terms_df)) > 0) {
                
                # get tidy_anger_col
                tidy_anger_col <- terms_df %>% select(element_id, sentence_id, anger) %>% unnest(anger) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "anger") %>% rename(term = anger)
                
                # get tidy_anticipation_col
                tidy_anticipation_col <- terms_df %>% select(element_id, sentence_id, anticipation) %>% unnest(anticipation) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "anticipation") %>% rename(term = anticipation)
                
                # get tidy_disgust_col
                tidy_disgust_col <- terms_df %>% select(element_id, sentence_id, disgust) %>% unnest(disgust) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "disgust") %>% rename(term = disgust)
                
                # get tidy_fear_col
                tidy_fear_col <- terms_df %>% select(element_id, sentence_id, fear) %>% unnest(fear) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "fear") %>% rename(term = fear)
                
                # get tidy_joy_col
                tidy_joy_col <- terms_df %>% select(element_id, sentence_id, joy) %>% unnest(joy) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "joy") %>% rename(term = joy)
                
                # get tidy_sadness_col
                tidy_sadness_col <- terms_df %>% select(element_id, sentence_id, sadness) %>% unnest(sadness) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "sadness") %>% rename(term = sadness)
                
                # get tidy_surprise_col
                tidy_surprise_col <- terms_df %>% select(element_id, sentence_id, surprise) %>% unnest(surprise) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "surprise") %>% rename(term = surprise)
                
                # get tidy_trust_col
                tidy_trust_col <- terms_df %>% select(element_id, sentence_id, trust) %>% unnest(trust) %>%
                        left_join(terms_df %>% select(element_id, sentence_id), ., by = c("element_id", "sentence_id")) %>%
                        mutate(emotion = "trust") %>% rename(term = trust)
                
                # combine tidy_negative/neutral/positive_col
                tidy_terms_tbl <- bind_rows(tidy_anger_col, tidy_anticipation_col, tidy_disgust_col,
                                            tidy_fear_col, tidy_joy_col, tidy_sadness_col,
                                            tidy_surprise_col, tidy_trust_col) %>%
                        arrange(element_id, sentence_id, emotion)
                
                return(tidy_terms_tbl)
        }
}


# get mytext_sentiment_terms_tbl 
mytext_sentiment_terms <- mytext_sentiment_terms %>% as_tibble() %>% convert_terms_to_tbl()
mytext_sentiment_terms 
mytext_sentiment_terms %>% filter(!is.na(term)) %>% group_by(element_id) %>% count(sentiment) %>% ungroup()
      
                                                              


################################


# join mytext_sentiment and mytext_sentiment_terms_tbl
# note the term_sentiment_matches_overall_sentiment_flag is for optional use
# for instance if you wanted to only inspect terms with a sentiment matching the overall sentence sentiment
mytext_sentiment_tbl <- mytext_sentiment_terms %>% 
        left_join(., mytext_sentiment %>% rename(overall_sentiment_score = sentiment), by = c("element_id", "sentence_id")) %>%
        mutate(overall_sentiment = case_when(overall_sentiment_score < 0 ~ "negative", 
                                                  overall_sentiment_score == 0 ~ "neutral",
                                                  overall_sentiment_score > 0 ~ "positive"),
               term_sentiment_matches_overall_sentiment_flag = case_when(sentiment == overall_sentiment ~ 1, TRUE ~ 0))
mytext_sentiment_tbl 

# note you could add a flag for negated terms, and below is some starter code
# but it's problematic because if you go down the negation rabbithole, you really don't know for sure what words are negated
# except in cases where there's only a single sentiment term, and it's sentiment is opposite the overall_sentiment
# and so there's probably diminishing returns to chasing that down
# plus, if you'll still have possible negations that can be confirmed in sentences with multiple sentiment terms matching the overall sentiment 
# so, since there's always going to be a big caveat about negations, it seems expedient to avoid the rabbit hole, at least for now
# mytext_sentiment_terms_tbl %>% group_by(element_id, sentence_id) %>%
#         mutate(sentence_has_negative_term_flag = case_when(sentiment == "negative" & !is.na(term) ~ 1, TRUE ~ 0),
#                sentence_has_negative_term_flag = case_when(sum(sentence_has_negative_term_flag) > 0 ~ 1, TRUE ~ 0),
#                sentence_has_positive_term_flag = case_when(sentiment == "positive" & !is.na(term) ~ 1, TRUE ~ 0),
#                sentence_has_positive_term_flag = case_when(sum(sentence_has_positive_term_flag) > 0 ~ 1, TRUE ~ 0),
#                sentence_negative_term_sum = sum(sentiment == "negative"),
#                sentence_positive_term_sum = sum(sentiment == "positive")) %>% 
#         ungroup() %>%
#         mutate(negated_negative_term_flag = case_when(overall_sentiment_type == "positive" &
#                                                               sentence_has_negative_term_flag == 1 & 
#                                                               sentence_has_positive_term_flag == 0 &
#                                                               sentence_negative_term_sum == 1 ~ 1, TRUE ~ 0))


###############################################################################################################################
##################################################################################################################
##################################################################################################################


# simple example of sentiment_by
# note that by = NULL default will make the strings the by group 
mytext_tbl %$% sentiment_by(text.var = sentences, by = NULL)

# this is equivalent
sentiment_by(mytext_sentences)


######################


# more complex example of sentiment_by
debates <- read_csv("presidential_debates_2012.csv")
debates

# get debate_sentiment_by_person_time
debates_sentiment_by_person_time <- debates %>% mutate(sentences = get_sentences(dialogue)) %$%
        sentiment_by(text.var = sentences, by = list(person, time))
debates_sentiment_by_person_time

# visualize highlighted positive/negative sentences
debates_sentiment_by_person_time %>% highlight()

# note that sentiment_by() is just shorthand for manually seperating by groups, running sentiment() on them individually, 
# and averaging sentiment with average_downweighted_zero(), sd is just sd()
# see equivalent ave_sentiment/sd scores for obama/time_1 below
obama_time_1_sentiment <- debates %>% filter(person == "OBAMA", time == "time 1") %>% mutate(sentences = get_sentences(dialogue)) %$%
        sentiment(text.var = sentences) %>% as_tibble() 

obama_time_1_sentiment_overall <- obama_time_1_sentiment %>% summarize(ave_sentiment = average_downweighted_zero(sentiment),
                                                                      sd = sd(sentiment))
obama_time_1_sentiment_overall

# while you can't add sentiment_terms wholesale to sentiment_by output, the same way you can for sentiment() output
# you could still do it if you manually ran sentiment() on all the by_groups individually
# for instance, get obama_time_1_sentiment_terms
obama_time_1_sentiment_terms <- debates %>% filter(person == "OBAMA", time == "time 1") %>%
        mutate(sentences = get_sentences(dialogue)) %$% extract_sentiment_terms(text.var = sentences) %>%
        as_tibble() %>% convert_terms_to_tbl()
obama_time_1_sentiment_terms

# combine obama_time_1_sentiment_terms and obama_time_1_sentiment
obama_time_1_sentiment_tbl <- obama_time_1_sentiment_terms %>% 
        left_join(., obama_time_1_sentiment %>% rename(overall_sentiment_score = sentiment), by = c("element_id", "sentence_id")) %>%
        mutate(overall_sentiment = case_when(overall_sentiment_score < 0 ~ "negative", 
                                             overall_sentiment_score == 0 ~ "neutral",
                                             overall_sentiment_score > 0 ~ "positive"),
               term_sentiment_matches_overall_sentiment_flag = case_when(sentiment == overall_sentiment ~ 1, TRUE ~ 0))
obama_time_1_sentiment_tbl


##################################################################################################################
##################################################################################################################
##################################################################################################################


# get emotion
# note emotion_count is the count of words in that sentence which coded as the given emotion
# note the "emotion" variable is really the "emotion rate" referenced in the docs, which is just emotion_count / word_count
mytext_emotion <- mytext_tbl %$% emotion(text.var = sentences) %>% as_tibble() %>% 
        mutate(emotion_type = as.character(emotion_type)) %>% rename(emotion_rate = emotion, emotion = emotion_type)
mytext_emotion
mytext_emotion %>% print(n = 20)
mytext_emotion %>% glimpse()
mytext_tbl

# note there are 16 emotion types
mytext_emotion %>% distinct(emotion)

# extract_emotion_terms
# note because the emotion variables are stored as lists for some reason, as_tibble chokes and tries to nest it
# so data.frame is used instead 
mytext_emotion_terms <- mytext_tbl %$% extract_emotion_terms(text.var = sentences) %>% as_tibble() %>% convert_terms_to_tbl()
mytext_emotion_terms
mytext_emotion_terms %>% glimpse()

# combine emotion and emotion_terms
mytext_emotion_tbl <- mytext_emotion_terms %>% 
        left_join(., mytext_emotion, by = c("element_id", "sentence_id", "emotion"))
mytext_emotion_tbl



