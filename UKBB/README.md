### UK Biobank data

Here we provide the codes for linearly adjusted transfer model UK Biobank data. The `clean.data` function in `mortality.r` file has the steps for data cleaning procedures. To get the results for the logistic regression model with main and two-way interaction effects we run `logistic.R` file. To get the results for random forest we use `randomForest` function from `randomForest` [1] package. To get the results for random forest model we run `RF_gini.R` followed by `RF.R`. 

---
#### References

[1] Leo Breiman, Adele Cutler, Andy Liaw and Matthew Wiener. "Package ‘randomForest’." University of California, Berkeley: Berkeley, CA, USA (2018).