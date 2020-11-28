Saturday, November 28, 2020
rf100 Version: 0.0.1

1) put the code in R package format at parsifal9/rf100
2) added code so fitted values returned for correct obervation
   a) removed the change in reg_tree_imp -- now all of the cahnges are in sprout_tree


######################################################################################################
Saturday, November 14, 2020

the original code was bagging the data 
## - randomly sample the data with replacement (duplicate are possible)
#   train <-
#     data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
and returning the fitted values for the bagged data.
So if the data was of size $n \timesp$ then $n$ fitted values are retuned but not
for the data, they are for a bagged version of the data.

This is returned from sprout_tree and mean of the fitted values is calculated.

##calculate the final fit as a mean of all regression trees
# rf_fit <- apply(fits, MARGIN = 1, mean, na.rm = TRUE)

Now how this is wrong. We are taking the mean of the fits for random data points.

To fix this

1) in sprout_tree, record which data points are in the bootstrap sample

#temp1<-sample(1:nrow(data), size = nrow(data), replace = TRUE)
#train <-      data[temp1,]


2) send the size of the data to reg_tree_imp
#tree <- reg_tree_imp(formula = formula_new,
#                         data = train,
#                         minsize = ceiling(nrow(train) * 0.1),data.n=dim(data)[[1]])

so reg_tree_imp can return a tree$fit of the correct size.



3)  in sprout_tree, group by temp1 and get the mean y value for each leaf. This is the fitted value
#  aa<-data.frame(tree$fit,temp1) %>%
#        dplyr::group_by(temp1)%>%
#        dplyr::summarize(Mean = mean(tree.fit, na.rm=TRUE))
#    aa<-as.matrix(aa)
#    tt<-rep(NA,length(temp1))
#    tt[aa[,1]]<-aa[,2]
#    # save the fit and the importance
#    return(list(tt, tree$importance))

So it works now.

We still have the problems
* all the warnings -- `summarise()` ungrouping output (override with `.groups` argument)
* we are getting the fitted values for the samples that were selected by the bootstrap ( the in-bag samples).
Would be better if we got the fitted values for the out-of-bag samples. (is this true?)
* the "randomly sample features" is done at the same level as the "bag the data". I think it should be done at each node