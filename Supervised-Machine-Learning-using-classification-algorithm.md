<https://github.com/edefojoshua/Supervised-Machine-Learning-using-classification-algorithm.git>
================
Joshua Edefo
2024-01-22

Libraries

``` r
library(caTools)
```

    ## Warning: package 'caTools' was built under R version 4.3.2

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.3.2

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 4.3.2

    ## Loading required package: lattice

Loading the data

``` r
# downloading data from the internet
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data<-read.csv(url, header = FALSE)
```

Data cleansing

``` r
head(data)
```

    ##   V1 V2 V3  V4  V5 V6 V7  V8 V9 V10 V11 V12 V13 V14
    ## 1 63  1  1 145 233  1  2 150  0 2.3   3 0.0 6.0   0
    ## 2 67  1  4 160 286  0  2 108  1 1.5   2 3.0 3.0   2
    ## 3 67  1  4 120 229  0  2 129  1 2.6   2 2.0 7.0   1
    ## 4 37  1  3 130 250  0  0 187  0 3.5   3 0.0 3.0   0
    ## 5 41  0  2 130 204  0  2 172  0 1.4   1 0.0 3.0   0
    ## 6 56  1  2 120 236  0  0 178  0 0.8   1 0.0 3.0   0

``` r
# unfortunately no columns are labelled, so we named the column
colnames(data)<-c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg","thalach", "exang", "oldpeak","slope","ca", "thal","hd")
head(data)
```

    ##   age sex cp trestbps chol fbs restecg thalach exang oldpeak slope  ca thal hd
    ## 1  63   1  1      145  233   1       2     150     0     2.3     3 0.0  6.0  0
    ## 2  67   1  4      160  286   0       2     108     1     1.5     2 3.0  3.0  2
    ## 3  67   1  4      120  229   0       2     129     1     2.6     2 2.0  7.0  1
    ## 4  37   1  3      130  250   0       0     187     0     3.5     3 0.0  3.0  0
    ## 5  41   0  2      130  204   0       2     172     0     1.4     1 0.0  3.0  0
    ## 6  56   1  2      120  236   0       0     178     0     0.8     1 0.0  3.0  0

``` r
# this tells us the 6 rows 0f the data

str(data)
```

    ## 'data.frame':    303 obs. of  14 variables:
    ##  $ age     : num  63 67 67 37 41 56 62 57 63 53 ...
    ##  $ sex     : num  1 1 1 1 0 1 0 0 1 1 ...
    ##  $ cp      : num  1 4 4 3 2 2 4 4 4 4 ...
    ##  $ trestbps: num  145 160 120 130 130 120 140 120 130 140 ...
    ##  $ chol    : num  233 286 229 250 204 236 268 354 254 203 ...
    ##  $ fbs     : num  1 0 0 0 0 0 0 0 0 1 ...
    ##  $ restecg : num  2 2 2 0 2 0 2 0 2 2 ...
    ##  $ thalach : num  150 108 129 187 172 178 160 163 147 155 ...
    ##  $ exang   : num  0 1 1 0 0 0 0 1 0 1 ...
    ##  $ oldpeak : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
    ##  $ slope   : num  3 2 2 3 1 1 3 1 2 3 ...
    ##  $ ca      : chr  "0.0" "3.0" "2.0" "0.0" ...
    ##  $ thal    : chr  "6.0" "3.0" "7.0" "3.0" ...
    ##  $ hd      : int  0 2 1 0 0 0 3 0 2 1 ...

``` r
# this tells us the structure of the data
# this tells us that some of the columns of the data are messed up  
#e.g sex is supposed to be factor (0=female, 1=male) data, cp(chest pain) also suppose to be factor representing different levels of pain
# ca and thal are correctly called factors but one of the levels is "?" when we need it to be NA
# so got some cleaning to do
# change the ? to n=NA
data[data=="?"] <- NA

# correct the sex values ans change sex to factor
data[data$sex==0,]$sex<-"F"
data[data$sex==1,]$sex<-"M"
data$sex<-as.factor(data$sex)

# convert  a bunch of other columns into factors since that is what they are supposed o be
data$cp <-as.factor(data$cp)
data$fbs <-as.factor(data$fbs)
data$restecg <-as.factor(data$restecg)
data$exang <-as.factor(data$exang)
data$slope <-as.factor(data$slope)

# R thought ca varaible was string whereas it is interger
data$ca <- as.integer(data$ca) # then covert to factor
data$ca <- as.factor(data$ca)

# do likewise for thal
data$thal <- as.integer(data$thal) # then covert to factor
data$thal <- as.factor(data$thal)

# hd 8heart disease) into a factor, using ifelse() to convert the 0s to "Healthy" and the 1s to "Unhealthy"
data$hd <-ifelse(tes = data$hd==0, yes="Healthy", no ="Unhealthy")
data$hd <- as.factor(data$hd)

# check if you have done the corrections
str(data)
```

    ## 'data.frame':    303 obs. of  14 variables:
    ##  $ age     : num  63 67 67 37 41 56 62 57 63 53 ...
    ##  $ sex     : Factor w/ 2 levels "F","M": 2 2 2 2 1 2 1 1 2 2 ...
    ##  $ cp      : Factor w/ 4 levels "1","2","3","4": 1 4 4 3 2 2 4 4 4 4 ...
    ##  $ trestbps: num  145 160 120 130 130 120 140 120 130 140 ...
    ##  $ chol    : num  233 286 229 250 204 236 268 354 254 203 ...
    ##  $ fbs     : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 2 ...
    ##  $ restecg : Factor w/ 3 levels "0","1","2": 3 3 3 1 3 1 3 1 3 3 ...
    ##  $ thalach : num  150 108 129 187 172 178 160 163 147 155 ...
    ##  $ exang   : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 2 1 2 ...
    ##  $ oldpeak : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
    ##  $ slope   : Factor w/ 3 levels "1","2","3": 3 2 2 3 1 1 3 1 2 3 ...
    ##  $ ca      : Factor w/ 4 levels "0","1","2","3": 1 4 3 1 1 1 3 1 2 1 ...
    ##  $ thal    : Factor w/ 3 levels "3","6","7": 2 1 3 1 1 1 1 1 3 3 ...
    ##  $ hd      : Factor w/ 2 levels "Healthy","Unhealthy": 1 2 2 1 1 1 2 1 2 2 ...

``` r
# check for no of rows of data with NA values
nrow(data[is.na(data),]) # 6 rows of data
```

    ## [1] 6

``` r
nrow(data[is.na(data$ca),]) # 4 rows of data from ca variables
```

    ## [1] 4

``` r
nrow(data[is.na(data$thal),]) 
```

    ## [1] 2

``` r
nrow(data[is.na(data$ca) | is.na(data$thal),])
```

    ## [1] 6

``` r
# we can view the missing values
data[is.na(data),] # general
```

    ##      age  sex   cp trestbps chol  fbs restecg thalach exang oldpeak slope   ca
    ## NA    NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ## NA.1  NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ## NA.2  NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ## NA.3  NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ## NA.4  NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ## NA.5  NA <NA> <NA>       NA   NA <NA>    <NA>      NA  <NA>      NA  <NA> <NA>
    ##      thal   hd
    ## NA   <NA> <NA>
    ## NA.1 <NA> <NA>
    ## NA.2 <NA> <NA>
    ## NA.3 <NA> <NA>
    ## NA.4 <NA> <NA>
    ## NA.5 <NA> <NA>

``` r
data[is.na(data$ca) | is.na(data$thal),]
```

    ##     age sex cp trestbps chol fbs restecg thalach exang oldpeak slope   ca thal
    ## 88   53   F  3      128  216   0       2     115     0     0.0     1    0 <NA>
    ## 167  52   M  3      138  223   0       0     169     0     0.0     1 <NA>    3
    ## 193  43   M  4      132  247   1       2     143     1     0.1     2 <NA>    7
    ## 267  52   M  4      128  204   1       0     156     1     1.0     2    0 <NA>
    ## 288  58   M  2      125  220   0       0     144     0     0.4     2 <NA>    7
    ## 303  38   M  3      138  175   0       0     173     0     0.0     1 <NA>    3
    ##            hd
    ## 88    Healthy
    ## 167   Healthy
    ## 193 Unhealthy
    ## 267 Unhealthy
    ## 288   Healthy
    ## 303   Healthy

``` r
nrow(data)
```

    ## [1] 303

``` r
# use delete wise
data<-na.omit(data)
nrow(data)
```

    ## [1] 297

``` r
# we need to make sure that healthy and diseased samples comes from each gender, 
# if only male samples have heart disease, we should probably remove all femeles from the model
xtabs(~hd + sex, data=data)
```

    ##            sex
    ## hd            F   M
    ##   Healthy    71  89
    ##   Unhealthy  25 112

``` r
# now let's cerify that all 4 levels of chest pain (cp) were reported for by bunch of patients
xtabs(~hd + cp, data=data)
```

    ##            cp
    ## hd            1   2   3   4
    ##   Healthy    16  40  65  39
    ##   Unhealthy   7   9  18 103

``` r
# and then we do the same thing for all of the boolean and categorical variables that we are using to predict heart disease
xtabs(~hd + fbs, data=data)
```

    ##            fbs
    ## hd            0   1
    ##   Healthy   137  23
    ##   Unhealthy 117  20

``` r
xtabs(~hd + restecg, data=data) # only 4 patients represent level 1. This could, potentially, get in the way of finding the best fitting line.
```

    ##            restecg
    ## hd           0  1  2
    ##   Healthy   92  1 67
    ##   Unhealthy 55  3 79

``` r
#however, for now we will just leave it in and use it to see what happens
xtabs(~hd + exang, data=data)
```

    ##            exang
    ## hd            0   1
    ##   Healthy   137  23
    ##   Unhealthy  63  74

``` r
xtabs(~hd + slope, data=data)
```

    ##            slope
    ## hd            1   2   3
    ##   Healthy   103  48   9
    ##   Unhealthy  36  89  12

``` r
xtabs(~hd + ca, data=data)
```

    ##            ca
    ## hd            0   1   2   3
    ##   Healthy   129  21   7   3
    ##   Unhealthy  45  44  31  17

``` r
xtabs(~hd + thal, data=data)
```

    ##            thal
    ## hd            3   6   7
    ##   Healthy   127   6  27
    ##   Unhealthy  37  12  88

Split dataset

``` r
split<- sample.split(data, SplitRatio = 0.8)
train<-subset(data, split=="TRUE")
str(train)
```

    ## 'data.frame':    233 obs. of  14 variables:
    ##  $ age     : num  63 67 37 41 56 62 57 63 56 56 ...
    ##  $ sex     : Factor w/ 2 levels "F","M": 2 2 2 1 2 1 1 2 1 2 ...
    ##  $ cp      : Factor w/ 4 levels "1","2","3","4": 1 4 3 2 2 4 4 4 2 3 ...
    ##  $ trestbps: num  145 160 130 130 120 140 120 130 140 130 ...
    ##  $ chol    : num  233 286 250 204 236 268 354 254 294 256 ...
    ##  $ fbs     : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 2 ...
    ##  $ restecg : Factor w/ 3 levels "0","1","2": 3 3 1 3 1 3 1 3 3 3 ...
    ##  $ thalach : num  150 108 187 172 178 160 163 147 153 142 ...
    ##  $ exang   : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ oldpeak : num  2.3 1.5 3.5 1.4 0.8 3.6 0.6 1.4 1.3 0.6 ...
    ##  $ slope   : Factor w/ 3 levels "1","2","3": 3 2 3 1 1 3 1 2 2 2 ...
    ##  $ ca      : Factor w/ 4 levels "0","1","2","3": 1 4 1 1 1 3 1 2 1 2 ...
    ##  $ thal    : Factor w/ 3 levels "3","6","7": 2 1 1 1 1 1 1 3 1 2 ...
    ##  $ hd      : Factor w/ 2 levels "Healthy","Unhealthy": 1 2 1 1 1 2 1 2 1 2 ...

``` r
test<-subset(data, split=="FALSE")
str(test)
```

    ## 'data.frame':    64 obs. of  14 variables:
    ##  $ age     : num  67 53 57 48 58 60 69 57 55 61 ...
    ##  $ sex     : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 1 2 2 1 ...
    ##  $ cp      : Factor w/ 4 levels "1","2","3","4": 4 4 4 2 3 4 1 4 4 4 ...
    ##  $ trestbps: num  120 140 140 110 132 130 140 150 132 130 ...
    ##  $ chol    : num  229 203 192 229 224 206 239 276 353 330 ...
    ##  $ fbs     : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 1 1 1 ...
    ##  $ restecg : Factor w/ 3 levels "0","1","2": 3 3 1 1 3 3 1 3 1 3 ...
    ##  $ thalach : num  129 155 148 168 173 132 151 112 132 169 ...
    ##  $ exang   : Factor w/ 2 levels "0","1": 2 2 1 1 1 2 1 2 2 1 ...
    ##  $ oldpeak : num  2.6 3.1 0.4 1 3.2 2.4 1.8 0.6 1.2 0 ...
    ##  $ slope   : Factor w/ 3 levels "1","2","3": 2 3 2 3 1 2 1 2 2 1 ...
    ##  $ ca      : Factor w/ 4 levels "0","1","2","3": 3 1 1 1 3 3 3 2 2 1 ...
    ##  $ thal    : Factor w/ 3 levels "3","6","7": 3 3 2 3 3 3 1 2 3 1 ...
    ##  $ hd      : Factor w/ 2 levels "Healthy","Unhealthy": 2 2 1 2 2 2 1 2 2 2 ...

Create the model

``` r
logistic_model<-glm(hd~., family="binomial",  data=train) 
summary(logistic_model)
```

    ## 
    ## Call:
    ## glm(formula = hd ~ ., family = "binomial", data = train)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.093e+00  3.248e+00  -1.260 0.207573    
    ## age         -4.306e-02  3.142e-02  -1.371 0.170514    
    ## sexM         2.082e+00  6.668e-01   3.122 0.001793 ** 
    ## cp2          9.599e-01  8.987e-01   1.068 0.285504    
    ## cp3          1.587e-01  7.794e-01   0.204 0.838692    
    ## cp4          1.931e+00  7.659e-01   2.521 0.011712 *  
    ## trestbps     2.401e-02  1.356e-02   1.771 0.076589 .  
    ## chol         5.757e-03  4.986e-03   1.154 0.248296    
    ## fbs1        -3.752e-01  6.797e-01  -0.552 0.580896    
    ## restecg1     1.434e+01  1.320e+03   0.011 0.991328    
    ## restecg2     4.710e-01  4.833e-01   0.975 0.329757    
    ## thalach     -2.594e-02  1.319e-02  -1.968 0.049109 *  
    ## exang1       4.941e-01  5.343e-01   0.925 0.355068    
    ## oldpeak      3.785e-01  2.928e-01   1.293 0.196180    
    ## slope2       1.145e+00  5.812e-01   1.971 0.048752 *  
    ## slope3      -3.821e-01  1.382e+00  -0.277 0.782137    
    ## ca1          2.075e+00  5.802e-01   3.576 0.000349 ***
    ## ca2          3.613e+00  9.534e-01   3.790 0.000151 ***
    ## ca3          2.367e+00  1.004e+00   2.358 0.018387 *  
    ## thal6        1.068e+00  1.148e+00   0.930 0.352253    
    ## thal7        1.458e+00  4.943e-01   2.949 0.003192 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 317.73  on 232  degrees of freedom
    ## Residual deviance: 136.78  on 212  degrees of freedom
    ## AIC: 178.78
    ## 
    ## Number of Fisher Scoring iterations: 15

Use the model as a predictive tool

``` r
predict<- predict(logistic_model, test, type="response")
predict
```

    ##           3          10          11          17          24          25 
    ## 0.997653672 0.728004678 0.612221148 0.112699174 0.914777429 0.998181859 
    ##          31          38          39          45          52          53 
    ## 0.124522723 0.994673483 0.992992767 0.024557435 0.268452000 0.723041381 
    ##          59          66          67          73          80          81 
    ## 0.232245969 0.999086828 0.217653348 0.998485486 0.918593418 0.746940210 
    ##          87          95          96         102         109         110 
    ## 0.065384869 0.002607305 0.888040866 0.025640048 0.989775600 0.853078217 
    ##         116         123         124         130         137         138 
    ## 0.594305712 0.120684250 0.951240852 0.007533165 0.667949730 0.977772905 
    ##         144         151         152         158         165         166 
    ## 0.634427437 0.305426214 0.211098489 0.962878673 0.315104243 0.358716022 
    ##         173         180         181         187         195         196 
    ## 0.238178038 0.164748608 0.796349302 0.031420102 0.027481222 0.981624487 
    ##         202         209         210         216         223         224 
    ## 0.097333158 0.056707905 0.160786080 0.301484693 0.001052598 0.999266181 
    ##         230         237         238         244         251         252 
    ## 0.654766439 0.864971602 0.678901226 0.870715299 0.780641303 0.992554850 
    ##         258         265         266         273         280         281 
    ## 0.999962200 0.979493885 0.965617442 0.999369714 0.080838222 0.990355754 
    ##         287         295         296         302 
    ## 0.987932109 0.065801817 0.022895828 0.126609102

``` r
str(predict)
```

    ##  Named num [1:64] 0.998 0.728 0.612 0.113 0.915 ...
    ##  - attr(*, "names")= chr [1:64] "3" "10" "11" "17" ...

Validate the model and check for accurary

``` r
# Validate the model
confMatrix<-table(Actual_value=test$hd, predicted_value = predict>0.5)
confMatrix
```

    ##             predicted_value
    ## Actual_value FALSE TRUE
    ##    Healthy      21    5
    ##    Unhealthy     7   31

``` r
# accuracy
(confMatrix[[1,1]] + confMatrix [[2,2]]) / sum(confMatrix)
```

    ## [1] 0.8125

Session Information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] caret_6.0-94   lattice_0.21-8 ggplot2_3.4.4  caTools_1.18.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] future_1.33.1        utf8_1.2.3           generics_0.1.3      
    ##  [4] class_7.3-22         bitops_1.0-7         stringi_1.7.12      
    ##  [7] pROC_1.18.5          listenv_0.9.0        digest_0.6.33       
    ## [10] magrittr_2.0.3       timechange_0.2.0     evaluate_0.21       
    ## [13] grid_4.3.1           iterators_1.0.14     fastmap_1.1.1       
    ## [16] foreach_1.5.2        plyr_1.8.9           Matrix_1.6-1.1      
    ## [19] ModelMetrics_1.2.2.2 nnet_7.3-19          survival_3.5-5      
    ## [22] purrr_1.0.2          fansi_1.0.4          scales_1.2.1        
    ## [25] codetools_0.2-19     lava_1.7.3           cli_3.6.1           
    ## [28] rlang_1.1.1          hardhat_1.3.0        parallelly_1.36.0   
    ## [31] future.apply_1.11.1  munsell_0.5.0        splines_4.3.1       
    ## [34] withr_2.5.0          yaml_2.3.7           prodlim_2023.08.28  
    ## [37] parallel_4.3.1       tools_4.3.1          reshape2_1.4.4      
    ## [40] dplyr_1.1.3          colorspace_2.1-0     recipes_1.0.9       
    ## [43] globals_0.16.2       vctrs_0.6.3          R6_2.5.1            
    ## [46] rpart_4.1.19         stats4_4.3.1         lubridate_1.9.3     
    ## [49] lifecycle_1.0.3      stringr_1.5.0        MASS_7.3-60         
    ## [52] pkgconfig_2.0.3      pillar_1.9.0         gtable_0.3.4        
    ## [55] glue_1.6.2           data.table_1.14.8    Rcpp_1.0.11         
    ## [58] xfun_0.40            tibble_3.2.1         tidyselect_1.2.0    
    ## [61] rstudioapi_0.15.0    knitr_1.44           htmltools_0.5.6     
    ## [64] nlme_3.1-162         rmarkdown_2.25       ipred_0.9-14        
    ## [67] timeDate_4032.109    gower_1.0.1          compiler_4.3.1
