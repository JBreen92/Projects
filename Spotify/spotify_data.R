library(tidyverse)
library(caret)
library(corrplot)
library(gbm)

spotify <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
spotify$playlist_genre <- as.factor(spotify$playlist_genre)
spotify$mode <- as.factor(spotify$mode)
spotify$key <- as.factor(spotify$key)

spotify1 <- spotify %>% select(playlist_genre:duration_ms) %>% select(-playlist_subgenre)

corrplot(cor(spotify1 %>% select(-playlist_genre, -key, -mode)), type = "upper", diag = FALSE)


set.seed(123)
inTrain1 <- createDataPartition(y = spotify1$playlist_genre, p = .7, list = FALSE)
training1 <- spotify1[inTrain1,]
testing1 <- spotify1[-inTrain1,]

control <- trainControl(method = "cv", number = 5)

rfGrid <- expand.grid(mtry = c(1:5))
gbmGrid <- expand.grid(interaction.depth = c(1, 3, 5), 
                       n.trees = (1:5)*100, 
                       shrinkage = 0.1, 
                       n.minobsinnode = c(5, 10 ,15))

mod_rf <- train(playlist_genre ~ ., data = training1, method = "rf", trControl = control,
                preProcess = c("center", "scale"), tuneGrid = rfGrid)

mod_gbm <- train(playlist_genre ~ ., data = training1, method = "gbm", trControl = control,
                 verbose = FALSE, preProcess = c("center", "scale"), tuneGrid = gbmGrid)

mod_knn <- train(playlist_genre ~ ., data = training1, method = "knn", trControl = control,
                 preProcess = c("center", "scale"))

mod_xgb <- train(playlist_genre ~ ., data = training1, method = "xgbTree", trControl = control,
                 preProcess = c("center", "scale"))

gbmGrid1 <- expand.grid(interaction.depth = c(5, 6, 7), 
                       n.trees = (2:4)*100, 
                       shrinkage = 0.1, 
                       n.minobsinnode = c(10, 15))

mod_gbm1 <- train(playlist_genre ~ ., data = training1, method = "gbm", trControl = control,
                 verbose = FALSE, preProcess = c("center", "scale"), tuneGrid = gbmGrid1)

#Looking at results from CV it appears that RF is the best option based on accuracy
#Might also be worth noting all the best models were almost the same

pred_rf <- predict(mod_rf, testing1)

results_rf <- confusionMatrix(pred_rf, testing1$playlist_genre)

spotify_track <- spotify %>% select(track_id, playlist_genre:duration_ms) %>% select(-playlist_subgenre)

spotify_track %>% group_by(playlist_genre) %>% summarise(proportion = n()/nrow(spotify_track))

multi <- spotify_track %>%
            group_by(track_id, playlist_genre) %>%
            count() %>%
            group_by(track_id) %>%
            summarise(count = n()) %>%
            filter(count >= 2)

spotify_multi <- right_join(spotify_track, multi, by = "track_id")

spotify_multi %>% 
      group_by(playlist_genre) %>%
      summarise(proportion = n()/nrow(spotify_multi))

#Ideas: Types of music least/most likely to get mixed up with each other 
#           "Rock music is the least likely to get mixed up with EDM"
#       Also consider removing pop category entirely and trying to model without it


set.seed(456)
spotify2 <- spotify1 %>% 
                group_by(playlist_genre) %>%
                slice_sample(n = 4900)

inTrain2 <- createDataPartition(y = spotify2$playlist_genre, p = .7, list = FALSE)
training2 <- spotify2[inTrain2,]
testing2 <- spotify2[-inTrain2,]

mod_rf2 <- train(playlist_genre ~ ., data = training2, method = "rf", trControl = control,
                preProcess = c("center", "scale"), tuneGrid = rfGrid)

mod_gbm2 <- train(playlist_genre ~ ., data = training2, method = "gbm", trControl = control,
                 verbose = FALSE, preProcess = c("center", "scale"), tuneGrid = gbmGrid)

pred_rf2 <- predict(mod_rf2, testing2)
results2 <- confusionMatrix(pred_rf2, testing2$playlist_genre)

#Idea to remove the pop category and see how that changes things
spotify3 <- spotify1 %>% filter(playlist_genre != "pop")
spotify3 <- droplevels(spotify3)

set.seed(789)
inTrain3 <- createDataPartition(y = spotify3$playlist_genre, p = .7, list = FALSE)
training3 <- spotify3[inTrain3,]
testing3 <- spotify3[-inTrain3,]

mod_rf3 <- train(playlist_genre ~ ., data = training3, method = "rf", trControl = control,
                 preProcess = c("center", "scale"), tuneGrid = rfGrid)

pred_rf3 <- predict(mod_rf3, testing3)
results3 <- confusionMatrix(pred_rf3, testing3$playlist_genre)

# Idea to do hypothesis test - genre isn't related to the number of different genres of a track
multi2 <- spotify_track %>%
              group_by(track_id, playlist_genre) %>%
              count() %>%
              group_by(track_id) %>%
              summarise(count = n())

spotify_hyp <- right_join(spotify_track, multi2, by = "track_id")

spotify_hyp <- spotify_hyp %>% 
                      select(playlist_genre, count)

chisq <- chisq.test(spotify_hyp$playlist_genre, spotify_hyp$count)

#Results show a weak, but statistically significant, relationship. Look at residuals for more information.

corrplot(chisq$residuals, is.cor = FALSE) #title it something!

contrib <- 100*chisq$residuals^2/chisq$statistic

test <- as.data.frame(contrib)
test <- test %>% rename(playlist_genre = spotify_hyp.playlist_genre, count = spotify_hyp.count)

