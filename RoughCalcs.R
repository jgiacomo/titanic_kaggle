titanicPrepPredicts <- function(df){
    
    # This function will prepare the predictor values of the titanic dataset. 
    # The main function will be to remove the unneccessary columns and then 
    # parameratize the factorable columns of the raw training and testing sets.
    
    # input: a data frame from the titanic dataset as hosted on kaggle.com
    # output: a data frame where the remaining columns are suited for subsiquent
    #         learning with a ML or neural network package.
    
    # I don't have time to create the function right now but I will endevour to
    # capture the essence of what I did and want in the function.
    
    library(dplyr)
    library(ggplot2)
    library(caret)
    # load any ML algorithms to be used
    
    trn <- read.csv("train.csv")
    tst <- read.csv("test.csv")
    
    # remove unwanted columns
    trn <- trn[,-c(1,4,9,11,12)]
    tst <- tst[,-c(1,4,9,11,12)]
    
    # factorize columns and create dummy variables
    trn$Sex <- as.factor(trn$Sex)
    trn$Pclass <- as.factor(trn$Pclass)
    tst$Sex <- as.factor(tst$Sex)
    tst$Pclass <- as.factor(tst$Pclass)
    
    # create model for dummy variables
    trn_dummyVars <- dummyVars(Survived ~ ., data=trn)
    
    # generate dummy variables using the model
    trn_df <- as.data.frame(predict(trn_dummyVars, newdata = trn))
    tst_df <- as.data.frame(predict(trn_dummyVars, newdata = tst))
    
    # Next we could look for highly correlated variables but I'm not sure it is
    # necessary for this particular dataset, so I'll skip it for now.
    
    # We need to replace missing Age values with computed guesses. I will
    # actually use ML regression algorithms to predict them.
    
    # Create model to predict Age using all data with Age values, train and
    # test.
    train_age <- trn_df %>% select(-Survived) %>% rbind(tst_df) %>%
        filter(!(is.na(Age)))
    # This is just an example, need to pick appropriate algorithm.
    AgeModel <- train(Age ~ ., data=train_age, method="cubist",
                      trControl=trainControl(method="cv",number=5))
    
    
    # predict ages for training data and recombine
    trn_noAge <- trn_df %>% filter(is.na(Age)) %>% select(-Age)
    trn_noAge$Age <- predict(AgeModel, newdata=trn_noAge)
    trn_df_age <- trn_df %>% filter(!(is.na(Age))) %>% rbind(trn_noAge)
    
    # predict ages for test data
    tst_noAge <- tst_df %>% filter(is.na(Age)) %>% select(-Age)
    tst_noAge$Age <- predict(AgeModel, newdata=tst_noAge)
    tst_df_age <- tst_df %>% filter(!(is.na(Age))) %>% rbind(tst_noAge)
    
    # Now that we have predicted ages for every passenger, we can train a
    # two-class ML model to predict survival.
    

}