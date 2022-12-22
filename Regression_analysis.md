Regression_Analysis
================

``` r
#install.packages('hydroTSM', 'dplyr', 'stringr', 'lmtest', 'olsrr', 'stargazer', 'stdmod', 'ggplot2', 'lubridate', 'car')

requiredPackages = c('hydroTSM', 'dplyr', 'stringr', 'lmtest', 'olsrr', 'stargazer', 'stdmod', 'ggplot2', 'lubridate', 'car')
suppressMessages(
 for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)){
   install.packages(p)
  }
  library(p, character.only = TRUE)
 }
)
```

    ## Warning: package 'zoo' was built under R version 4.1.2

    ## Warning: package 'lmtest' was built under R version 4.1.2

    ## Warning: package 'stdmod' was built under R version 4.1.2

    ## Warning: package 'lubridate' was built under R version 4.1.2

    ## Warning: package 'timechange' was built under R version 4.1.2

``` r
data <- read.csv('netflix_top10movies.csv', sep = ';')
```

``` r
#change data types
data$date_top10 <- as.Date(data$date_top10, format="%d-%m-%Y")
data$release_netflix<- as.Date(data$release_netflix, format="%d-%m-%Y")
data$release_theater<- as.Date(data$release_theater, format="%d-%m-%Y")
```

``` r
#keep rows where release date on netflix is not empty 
df <- data[!(is.na(data$release_netflix)), ]
```

``` r
#seasonality dummies 
df$seasonality <- time2season(df$release_netflix, out.fmt = 'seasons')
df$Release_summer <- ifelse(df$seasonality == "summer", 1,0)
df$Release_fall <- ifelse(df$seasonality == "autumm", 1,0)
df$Release_winter <- ifelse(df$seasonality == "winter", 1,0)
df$Release_spring <- ifelse(df$seasonality == "spring", 1,0)
```

``` r
#box office performance in millions
df$box_office_performance <- as.numeric(df$box_office_performance) / 1000000
df$box_office_performance <- round(df$box_office_performance, 2)
```

``` r
#rename columns
df <- df %>%
  rename("Holdback_period" = "holdback_months", 'Box_office_performance' = 'box_office_performance', 'Online_ratings' = 'meta_rating', 'Viewership_on_Netflix' = 'viewership')
```

``` r
#model
modelzl <- lm(Viewership_on_Netflix ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall + Holdback_period*Box_office_performance, data = df)
```

``` r
summary(modelzl)
```

    ## 
    ## Call:
    ## lm(formula = Viewership_on_Netflix ~ Holdback_period + Box_office_performance + 
    ##     Online_ratings + Release_spring + Release_summer + Release_fall + 
    ##     Holdback_period * Box_office_performance, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.358  -5.344  -1.206   3.552  80.529 
    ## 
    ## Coefficients:
    ##                                          Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                             1.187e+01  3.165e+00   3.751 0.000231
    ## Holdback_period                        -4.124e-03  9.628e-03  -0.428 0.668841
    ## Box_office_performance                  1.065e-01  1.796e-02   5.929 1.32e-08
    ## Online_ratings                         -1.104e-01  5.021e-02  -2.198 0.029104
    ## Release_spring                          2.673e+00  2.070e+00   1.291 0.198071
    ## Release_summer                         -1.836e+00  2.108e+00  -0.871 0.384836
    ## Release_fall                           -2.068e+00  2.239e+00  -0.924 0.356833
    ## Holdback_period:Box_office_performance -2.359e-04  8.875e-05  -2.659 0.008483
    ##                                           
    ## (Intercept)                            ***
    ## Holdback_period                           
    ## Box_office_performance                 ***
    ## Online_ratings                         *  
    ## Release_spring                            
    ## Release_summer                            
    ## Release_fall                              
    ## Holdback_period:Box_office_performance ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.93 on 200 degrees of freedom
    ## Multiple R-squared:  0.2676, Adjusted R-squared:  0.242 
    ## F-statistic: 10.44 on 7 and 200 DF,  p-value: 3.725e-11

## Assumptions

``` r
par(mfrow = c(2, 2))
plot(modelzl)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#linearity
plot(modelzl, 1)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#homoskedasticity
plot(modelzl, 3)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
lmtest::bptest(modelzl)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  modelzl
    ## BP = 26.496, df = 7, p-value = 0.0004105

``` r
#homoskedasticity after log transformation
model <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall + Holdback_period*Box_office_performance, data = df)
plot(model, 3)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#independence 
dwtest(formula = model, alternative = 'two.sided')
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model
    ## DW = 1.8484, p-value = 0.2141
    ## alternative hypothesis: true autocorrelation is not 0

``` r
#normality 
plot(model, 2)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
hist(model$residuals, main = 'Residual Histogram')
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
ols_test_normality(model)
```

    ## -----------------------------------------------
    ##        Test             Statistic       pvalue  
    ## -----------------------------------------------
    ## Shapiro-Wilk              0.9798         0.0044 
    ## Kolmogorov-Smirnov        0.0612         0.4178 
    ## Cramer-von Mises         14.0037         0.0000 
    ## Anderson-Darling          1.2053         0.0038 
    ## -----------------------------------------------

``` r
#multicollinearity
modelmulti <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_summer + Release_winter + Release_fall, data = df)
car::vif(modelmulti)
```

    ##        Holdback_period Box_office_performance         Online_ratings 
    ##               1.096987               1.236177               1.220121 
    ##         Release_summer         Release_winter           Release_fall 
    ##               1.478708               1.511375               1.541675

``` r
vif_values <- vif(modelmulti)

barplot(vif_values, main = "VIF Values", horiz = FALSE, col = "steelblue", cex.names = 0.4, las = 2)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# outliers
cooksd <- cooks.distance(model)
n <- nrow(df)
influential_obs <- as.numeric(names(cooksd)[cooksd > (4/n)])
influential_obs
```

    ##  [1]   8  25  45  75  81  87  96 109 112 113 139 141 154 166 179 184 200

``` r
df_withoutoutliers <- df[-influential_obs, ]


ols_plot_resid_lev(model)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ols_plot_cooksd_bar(model, print_plot = TRUE)
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Analysis

``` r
model1 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall, data = df)
model2 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall  + Holdback_period*Box_office_performance, data = df)
model3 <- lm(log(Viewership_on_Netflix) ~ Online_ratings + Release_spring + Release_summer + Release_fall, data = df)
```

``` r
stargazer(model1, model2, model3, title = 'Results', align = TRUE, type = 'text')
```

    ## 
    ## Results
    ## ==========================================================================================================
    ##                                                                Dependent variable:                        
    ##                                        -------------------------------------------------------------------
    ##                                                            log(Viewership_on_Netflix)                     
    ##                                                 (1)                    (2)                    (3)         
    ## ----------------------------------------------------------------------------------------------------------
    ## Holdback_period                               -0.001**                -0.001                              
    ##                                               (0.001)                (0.001)                              
    ##                                                                                                           
    ## Box_office_performance                        0.004***               0.006***                             
    ##                                               (0.001)                (0.001)                              
    ##                                                                                                           
    ## Online_ratings                               -0.011***              -0.010***               -0.003        
    ##                                               (0.004)                (0.004)                (0.004)       
    ##                                                                                                           
    ## Release_spring                                 0.270*                 0.282*                 0.238        
    ##                                               (0.146)                (0.145)                (0.158)       
    ##                                                                                                           
    ## Release_summer                                 -0.047                 -0.032                -0.029        
    ##                                               (0.148)                (0.148)                (0.161)       
    ##                                                                                                           
    ## Release_fall                                  -0.315**               -0.290*               -0.397**       
    ##                                               (0.157)                (0.157)                (0.167)       
    ##                                                                                                           
    ## Holdback_period:Box_office_performance                              -0.00001*                             
    ##                                                                     (0.00001)                             
    ##                                                                                                           
    ## Constant                                      2.482***               2.363***              2.259***       
    ##                                               (0.212)                (0.222)                (0.222)       
    ##                                                                                                           
    ## ----------------------------------------------------------------------------------------------------------
    ## Observations                                    208                    208                    208         
    ## R2                                             0.215                  0.227                  0.063        
    ## Adjusted R2                                    0.192                  0.200                  0.045        
    ## Residual Std. Error                       0.770 (df = 201)       0.767 (df = 200)      0.838 (df = 203)   
    ## F Statistic                            9.194*** (df = 6; 201) 8.387*** (df = 7; 200) 3.412** (df = 4; 203)
    ## ==========================================================================================================
    ## Note:                                                                          *p<0.1; **p<0.05; ***p<0.01

``` r
#graph interaction effect 
modelmod<- lm(Viewership_on_Netflix ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall  + Holdback_period*Box_office_performance, data = df)


p <- plotmod(modelmod, x = 'Holdback_period', w = 'Box_office_performance', x_label = 'Holdback period (months)', y_label = 'Viewership on Netflix', w_label = 'Box office performance')
p + theme_classic()
```

![](Regression_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Analysis without outliers

``` r
df_withoutoutliers <- data.frame(df_withoutoutliers)
```

``` r
model1 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall, data = df_withoutoutliers)
model2 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall  + Holdback_period*Box_office_performance, data = df_withoutoutliers)
```

``` r
stargazer(model1, model2, title = 'Results', align = TRUE, type = 'text')
```

    ## 
    ## Results
    ## ====================================================================================
    ##                                                     Dependent variable:             
    ##                                        ---------------------------------------------
    ##                                                 log(Viewership_on_Netflix)          
    ##                                                 (1)                    (2)          
    ## ------------------------------------------------------------------------------------
    ## Holdback_period                                -0.001                -0.0003        
    ##                                               (0.001)                (0.001)        
    ##                                                                                     
    ## Box_office_performance                        0.004***               0.005***       
    ##                                               (0.001)                (0.001)        
    ##                                                                                     
    ## Online_ratings                                -0.006*                -0.007**       
    ##                                               (0.003)                (0.003)        
    ##                                                                                     
    ## Release_spring                                 0.182                  0.206         
    ##                                               (0.130)                (0.131)        
    ##                                                                                     
    ## Release_summer                                 -0.033                 -0.022        
    ##                                               (0.133)                (0.133)        
    ##                                                                                     
    ## Release_fall                                 -0.381***              -0.376***       
    ##                                               (0.138)                (0.138)        
    ##                                                                                     
    ## Holdback_period:Box_office_performance                               -0.00001       
    ##                                                                     (0.00001)       
    ##                                                                                     
    ## Constant                                      2.301***               2.235***       
    ##                                               (0.196)                (0.201)        
    ##                                                                                     
    ## ------------------------------------------------------------------------------------
    ## Observations                                    191                    191          
    ## R2                                             0.224                  0.231         
    ## Adjusted R2                                    0.198                  0.201         
    ## Residual Std. Error                       0.656 (df = 184)       0.655 (df = 183)   
    ## F Statistic                            8.832*** (df = 6; 184) 7.847*** (df = 7; 183)
    ## ====================================================================================
    ## Note:                                                    *p<0.1; **p<0.05; ***p<0.01

## Analysis with missing release date on Netflix imputation

``` r
#select where release date on Netflix is missing and impute 
df1 <- data[is.na(data$release_netflix), ]  
df1$release_netflix <- df1$date_top10 %m-% months(6) 

#append two dataframes
df2 <- data[!is.na(data$release_netflix), ]  
df1 <- rbind(df1, df2)
```

``` r
#seasonality dummies 
df1$seasonality <- time2season(df1$release_netflix, out.fmt = 'seasons')
df1$Release_summer <- ifelse(df1$seasonality == "summer", 1,0)
df1$Release_fall <- ifelse(df1$seasonality == "autumm", 1,0)
df1$Release_winter <- ifelse(df1$seasonality == "winter", 1,0)
df1$Release_spring <- ifelse(df1$seasonality == "spring", 1,0)
```

``` r
#box office performance in millions
df1$box_office_performance <- as.numeric(df1$box_office_performance) / 1000000
df1$box_office_performance <- round(df1$box_office_performance, 2)
```

``` r
#rename columns
df1 <- df1 %>%
  rename("Holdback_period" = "holdback_months", 'Box_office_performance' = 'box_office_performance', 'Online_ratings' = 'meta_rating', 'Viewership_on_Netflix' = 'viewership')
```

``` r
model1 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall, data = df1)
model2 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall  + Holdback_period*Box_office_performance, data = df1)
```

``` r
stargazer(model1, model2, title = 'Results', align = TRUE, type = 'text')
```

    ## 
    ## Results
    ## ====================================================================================
    ##                                                     Dependent variable:             
    ##                                        ---------------------------------------------
    ##                                                 log(Viewership_on_Netflix)          
    ##                                                 (1)                    (2)          
    ## ------------------------------------------------------------------------------------
    ## Holdback_period                               -0.001**               -0.0005        
    ##                                               (0.0005)               (0.001)        
    ##                                                                                     
    ## Box_office_performance                        0.004***               0.005***       
    ##                                               (0.001)                (0.001)        
    ##                                                                                     
    ## Online_ratings                                 -0.005                 -0.004        
    ##                                               (0.003)                (0.003)        
    ##                                                                                     
    ## Release_spring                                 -0.068                 -0.055        
    ##                                               (0.137)                (0.137)        
    ##                                                                                     
    ## Release_summer                                 -0.197                 -0.187        
    ##                                               (0.137)                (0.137)        
    ##                                                                                     
    ## Release_fall                                  -0.275*                -0.262*        
    ##                                               (0.145)                (0.145)        
    ##                                                                                     
    ## Holdback_period:Box_office_performance                               -0.00001       
    ##                                                                     (0.00001)       
    ##                                                                                     
    ## Constant                                      2.131***               2.010***       
    ##                                               (0.190)                (0.203)        
    ##                                                                                     
    ## ------------------------------------------------------------------------------------
    ## Observations                                    309                    309          
    ## R2                                             0.149                  0.156         
    ## Adjusted R2                                    0.132                  0.137         
    ## Residual Std. Error                       0.856 (df = 302)       0.854 (df = 301)   
    ## F Statistic                            8.803*** (df = 6; 302) 7.969*** (df = 7; 301)
    ## ====================================================================================
    ## Note:                                                    *p<0.1; **p<0.05; ***p<0.01

# Analysis without outliers

``` r
cooksd <- cooks.distance(model2)
n <- nrow(df1)
influential_obs <- as.numeric(names(cooksd)[cooksd > (4/n)])
influential_obs
```

    ##  [1] 210 211 239 242 256 259 260 281 289 294 306   8  45  75  96 109 112 113 141
    ## [20] 179 184

``` r
df1_withoutoutliers <- df1[-influential_obs, ]
```

``` r
df1_withoutoutliers <- data.frame(df1_withoutoutliers)
```

``` r
model1 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall, data = df1_withoutoutliers)
model2 <- lm(log(Viewership_on_Netflix) ~ Holdback_period + Box_office_performance + Online_ratings + Release_spring + Release_summer + Release_fall  + Holdback_period*Box_office_performance, data = df1_withoutoutliers)
```

``` r
stargazer(model1, model2, title = 'Results', align = TRUE, type = 'text')
```

    ## 
    ## Results
    ## ====================================================================================
    ##                                                     Dependent variable:             
    ##                                        ---------------------------------------------
    ##                                                 log(Viewership_on_Netflix)          
    ##                                                 (1)                    (2)          
    ## ------------------------------------------------------------------------------------
    ## Holdback_period                               -0.001**                -0.001        
    ##                                               (0.0005)               (0.001)        
    ##                                                                                     
    ## Box_office_performance                        0.004***               0.005***       
    ##                                               (0.001)                (0.001)        
    ##                                                                                     
    ## Online_ratings                                 -0.003                 -0.003        
    ##                                               (0.003)                (0.003)        
    ##                                                                                     
    ## Release_spring                                 -0.045                 -0.036        
    ##                                               (0.140)                (0.140)        
    ##                                                                                     
    ## Release_summer                                 -0.168                 -0.163        
    ##                                               (0.139)                (0.139)        
    ##                                                                                     
    ## Release_fall                                   -0.224                 -0.217        
    ##                                               (0.148)                (0.147)        
    ##                                                                                     
    ## Holdback_period:Box_office_performance                               -0.00001       
    ##                                                                     (0.00001)       
    ##                                                                                     
    ## Constant                                      2.062***               1.962***       
    ##                                               (0.189)                (0.202)        
    ##                                                                                     
    ## ------------------------------------------------------------------------------------
    ## Observations                                    288                    288          
    ## R2                                             0.155                  0.161         
    ## Adjusted R2                                    0.137                  0.140         
    ## Residual Std. Error                       0.830 (df = 281)       0.828 (df = 280)   
    ## F Statistic                            8.590*** (df = 6; 281) 7.664*** (df = 7; 280)
    ## ====================================================================================
    ## Note:                                                    *p<0.1; **p<0.05; ***p<0.01
