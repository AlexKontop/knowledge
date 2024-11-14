install.packages("ggplot2")
install.packages("tm")
install.packages("textdata")
install.packages("tidytext")
install.packages("stopwords")
install.packages("rlang")
install.packages("dplyr")
install.packages("e1071")
install.packages("caret")
install.packages("rpart")

# Φόρτωση βιβλιοθηκών
library(ggplot2)
library(tm)
library(textdata)
library(tidytext)
library(stopwords)
library(rlang)
library(dplyr)
library(e1071)
library(caret)  # Για αξιολόγηση μοντέλου
library(rpart)  # Για Decision Tree


# 1. Διαβάζουμε το αρχείο CSV
sms_data <- read.csv("spam.csv", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1")

# 2. Προεπεξεργασία των δεδομένων

# Μετατροπή του κειμένου σε πεζά γράμματα
sms_data$v2 <- tolower(sms_data$v2)

# Αφαίρεση αριθμών και σημάτων στίξης
sms_data$v2 <- gsub("[^a-zA-Z ]", "", sms_data$v2)

# Αφαίρεση stopwords
sms_data$v2 <- removeWords(sms_data$v2, stopwords("en"))

# Αφαίρεση επιπλέον κενών
sms_data$v2 <- stripWhitespace(sms_data$v2)

# 3. Διαχωρισμός των δεδομένων σε εκπαιδευτικό και δοκιμαστικό σύνολο
set.seed(123)  # Για αναπαραγωγιμότητα
trainIndex <- createDataPartition(sms_data$v1, p = 0.7, list = FALSE)
train_data <- sms_data[trainIndex, ]
test_data <- sms_data[-trainIndex, ]

# 4. Δημιουργία Document-Term Matrix (DTM) για τα δεδομένα εκπαίδευσης
corpus <- Corpus(VectorSource(train_data$v2))
dtm <- DocumentTermMatrix(corpus)

# Δημιουργία DataFrame από τον DTM, προσθέτουμε τη στήλη-στόχο 'v1' (HAM ή SPAM)
dtm_matrix <- as.data.frame(as.matrix(dtm))
dtm_matrix$v1 <- train_data$v1  # Προσθήκη της στήλης-στόχου για το train set

# 5. Δημιουργία DTM για το δοκιμαστικό σύνολο
corpus_test <- Corpus(VectorSource(test_data$v2))

# Εφαρμογή του ίδιου λεξικού (dictionary) για το δοκιμαστικό σύνολο
dtm_test <- DocumentTermMatrix(corpus_test, control = list(dictionary = Terms(dtm)))

# Δημιουργία Matrix από τα δεδομένα δοκιμής
dtm_test_matrix <- as.data.frame(as.matrix(dtm_test))

# 6. Εκπαίδευση του μοντέλου Decision Tree με τον DTM
tree_model <- rpart(as.factor(v1) ~ ., data = dtm_matrix)

# 7. Πρόβλεψη με το μοντέλο Decision Tree
tree_predictions <- predict(tree_model, dtm_test_matrix, type = "class")

# 8. Αξιολόγηση του μοντέλου Decision Tree
tree_confusion <- confusionMatrix(tree_predictions, as.factor(test_data$v1))
print(tree_confusion)



# 9. Παράδειγμα εισόδου από τον χρήστη
user_message <- "Don't forget your meeting at 3 PM tomorrow."

# Ορισμός της user_message2
user_message2 <- user_message

# Προεπεξεργασία του μηνύματος, όπως ακριβώς για τα δεδομένα εκπαίδευσης
user_message <- tolower(user_message)  # Μετατροπή σε πεζά γράμματα
user_message <- gsub("[^a-zA-Z ]", "", user_message)  # Αφαίρεση αριθμών και σημάτων στίξης
user_message <- removeWords(user_message, stopwords("en"))  # Αφαίρεση stopwords
user_message <- stripWhitespace(user_message)  # Αφαίρεση επιπλέον κενών

# Δημιουργία Document-Term Matrix (DTM) για το μήνυμα του χρήστη
corpus_user <- Corpus(VectorSource(user_message))
dtm_user <- DocumentTermMatrix(corpus_user, control = list(dictionary = Terms(dtm)))

# Δημιουργία Matrix από τα δεδομένα του χρήστη
dtm_user_matrix <- as.data.frame(as.matrix(dtm_user))

# Πρόβλεψη με το εκπαιδευμένο μοντέλο Decision Tree
tree_prediction_user <- predict(tree_model, dtm_user_matrix, type = "class")

# Αντιστοίχιση των αριθμητικών τιμών στις κατηγορίες
tree_prediction_user_label <- ifelse(tree_prediction_user == "ham", "ham", "spam")

# Εμφάνιση του αποτελέσματος
cat("Το SMS:", user_message2, "είναι:", tree_prediction_user_label, "\n")


