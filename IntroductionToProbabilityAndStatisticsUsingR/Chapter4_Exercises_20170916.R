# Bayes rules
# P(Working | Accuracy)
# prior = each workers probs of working
prior <- c(0.6, 0.3, 0.1)
# like = each workers liklihood of error 
like <- c(0.003, 0.007, 0.01)
# working and liklihood of error
post <- prior * like
# law of total probabilities
sum <- sum(post)
post/sum

# P(Working | Accuracy) 8 samples
# prior = each workers probs of working
prior <- c(0.6, 0.3, 0.1)
# like = each workers liklihood of error 
like <- c(0.003, 0.007, 0.01)
# doing 8 samples in a row
post <- prior * like^8
post/sum(post)