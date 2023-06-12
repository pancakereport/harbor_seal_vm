library(tidyverse)
library(rjson)

test_actual <- read_csv("test/_annotations.csv", col_names = F)
names(test_actual) <- c("Tile", "X1", "Y1", "X2", "Y2", "class")
test_actual$actual <- TRUE

test_actual_counts <- test_actual %>% 
  group_by(Tile) %>%
  #count(class)
  summarise(actual_count=n())


test_predicted_json <- fromJSON(file="test_detections.json")

test_predicted <- data.frame(matrix(ncol=7, nrow=0))
names(test_predicted) <- names(test_actual) <- c("Tile", "X1", "Y1", "X2", "Y2", "class", "actual")
test_predicted <- transform(test_predicted, Tile = as.character(Tile), 
                            X1 = as.numeric(X1), Y1 = as.numeric(Y1),
                            X2 = as.numeric(X2), Y2 = as.numeric(Y2),
                            class = as.character(class))

for (image in names(test_predicted_json)) {
  print(image)
  for (anno in test_predicted_json[[image]]) {
    
    #test_predicted_json[[image]][[1]]$label
    new_row = data.frame(Tile = substring(image, 6),
                         X1 = anno$box[1], 
                         Y1 = anno$box[2], 
                         X2 = anno$box[3], 
                         Y2 = anno$box[4], 
                         class = "adult", 
                         actual = FALSE)
    test_predicted <- rows_append(test_predicted, new_row)
  }
  
  
}

test_predicted_counts <- test_predicted %>%
  group_by(Tile) %>%
  summarise(pred_count=n())

test <- full_join(test_predicted_counts, test_actual_counts)



p <- test %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

p

# image field in the tables


no_seals <- filter(test, is.na(actual_count))

too_many_seals <- filter(test, pred_count - actual_count > 0)

not_enough_seals <- filter(test, actual_count - pred_count > 0)
