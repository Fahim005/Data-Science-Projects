hcv <- read.csv("D:/hcvdata.csv",header = TRUE,sep = ",")
hcv

View(hcv)

str(hcv)

head(hcv)
tail(hcv)

summary(hcv)


missing_values <- colSums(is.na(hcv))
print(missing_values)

hcv <- hcv %>%
mutate_at(vars(ALB:PROT), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
mutate(Category = ifelse(is.na(Category), mode(Category, na.rm = TRUE), Category))
View(hcv)

missing_values <- colSums(is.na(hcv))
print(missing_values)

hcv <- hcv %>%
mutate_if(is.character, as.factor) %>% 
mutate(across(where(is.factor), ~ as.integer(factor(.))))
View(hcv)


scale(hcv)
str(hcv)



correlation_matrix <- cor(hcv[, 2:ncol(hcv)], hcv$Category)
hcv

corrplot(correlation_matrix, method = "color")

correlation_threshold <- 0.2
important_attributes <- names(hcv[, 2:ncol(hcv)])[apply(correlation_matrix, 1, function(x) any(abs(x) > correlation_threshold))]
important_attributes

important_att <- hcv %>% select(all_of(c(important_attributes, "Category")))
important_att

set.seed(123)  
train_size <- 0.8 
train_index <- sample(nrow(important_att), round(train_size * nrow(important_att)))
train_hcv <- important_att[train_index, ]
test_hcv <- important_att[-train_index, ]


train_features <- train_hcv[, -ncol(train_hcv)]
train_labels <- train_hcv$Category
test_features <- test_hcv[, -ncol(test_hcv)]
test_labels <- test_hcv$Category

k <- 3
knn_pred <- knn(train_features, test_features, train_labels, k)
knn_pred


accuracy <- sum(knn_pred == test_labels) / length(test_labels)
print(paste("Accuracy (Training/Test Set Approach):", accuracy))


knn_cv <- knn.cv(important_att[, -ncol(important_att)], important_att$Category, k = k)


accuracy_cv <- sum(knn_cv == important_att$Category) / length(important_att$Category)
print(paste("Accuracy (10-Fold Cross Validation):", accuracy_cv))


conf_matrix <- table(knn_cv, important_att$Category)
conf_matrix

recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
precision

print("Confusion Matrix:")
print(conf_matrix)
print(paste("Recall:", recall))
print(paste("Precision:", precision))
