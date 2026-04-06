Week 6-Binary response
================
Li-Hsin Chien
2026-04-02

### Model fitting

``` r
library(faraway)

wcgs$bmi <- with(wcgs, 703*wcgs$weight/(wcgs$height^2))
lmod <- glm(chd ~ age + height + weight +bmi + sdp + dbp + chol + dibep + cigs +arcus, family=binomial, wcgs)
lmodr <- step(lmod, trace=0)
summary(lmodr)
```

    ## 
    ## Call:
    ## glm(formula = chd ~ age + height + bmi + sdp + chol + dibep + 
    ##     cigs + arcus, family = binomial, data = wcgs)
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -15.299983   2.294134  -6.669 2.57e-11 ***
    ## age            0.061590   0.012397   4.968 6.76e-07 ***
    ## height         0.050161   0.027824   1.803   0.0714 .  
    ## bmi            0.060385   0.026599   2.270   0.0232 *  
    ## sdp            0.017728   0.004155   4.267 1.98e-05 ***
    ## chol           0.010709   0.001529   7.006 2.45e-12 ***
    ## dibepB        -0.657616   0.145898  -4.507 6.56e-06 ***
    ## cigs           0.021041   0.004262   4.936 7.96e-07 ***
    ## arcuspresent   0.210998   0.143718   1.468   0.1421    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1769.2  on 3139  degrees of freedom
    ## Residual deviance: 1569.3  on 3131  degrees of freedom
    ##   (14 observations deleted due to missingness)
    ## AIC: 1587.3
    ## 
    ## Number of Fisher Scoring iterations: 6

### Prediction

![\sqrt{n}(\hat{\beta}-\beta_0) \sim N(0, I^{-1}(\beta))](https://latex.codecogs.com/png.latex?%5Csqrt%7Bn%7D%28%5Chat%7B%5Cbeta%7D-%5Cbeta_0%29%20%5Csim%20N%280%2C%20I%5E%7B-1%7D%28%5Cbeta%29%29 "\sqrt{n}(\hat{\beta}-\beta_0) \sim N(0, I^{-1}(\beta))")

![\hat{\beta} \sim N(\beta_0, \frac{1}{n} I^{-1}(\hat{\beta}))](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cbeta%7D%20%5Csim%20N%28%5Cbeta_0%2C%20%5Cfrac%7B1%7D%7Bn%7D%20I%5E%7B-1%7D%28%5Chat%7B%5Cbeta%7D%29%29 "\hat{\beta} \sim N(\beta_0, \frac{1}{n} I^{-1}(\hat{\beta}))")

![\hat{\eta}=x_0 ^T \hat{\beta} \sim N( x_0 ^T\beta_0, \frac{1}{n}  x_0 ^T I^{-1}(\hat{\beta} )x_0)](https://latex.codecogs.com/png.latex?%5Chat%7B%5Ceta%7D%3Dx_0%20%5ET%20%5Chat%7B%5Cbeta%7D%20%5Csim%20N%28%20x_0%20%5ET%5Cbeta_0%2C%20%5Cfrac%7B1%7D%7Bn%7D%20%20x_0%20%5ET%20I%5E%7B-1%7D%28%5Chat%7B%5Cbeta%7D%20%29x_0%29 "\hat{\eta}=x_0 ^T \hat{\beta} \sim N( x_0 ^T\beta_0, \frac{1}{n}  x_0 ^T I^{-1}(\hat{\beta} )x_0)")

![(1-\alpha)%](https://latex.codecogs.com/png.latex?%281-%5Calpha%29%25 "(1-\alpha)%")
CI for
![\hat{\eta}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Ceta%7D "\hat{\eta}"):

![(\hat{\eta}\_L, \hat{\eta}\_U),\~\~~\text{where } \hat{\eta}\_L=\hat{\eta} - z^{\alpha/2} se(\hat{\eta}),\~\~~ \hat{\eta}\_U=\hat{\eta} + z^{\alpha/2} se(\hat{\eta}), \~\~~ se(\hat{\eta})=\frac{1}{n}  x_0 ^T I^{-1}(\hat{\beta} )x_0](https://latex.codecogs.com/png.latex?%28%5Chat%7B%5Ceta%7D_L%2C%20%5Chat%7B%5Ceta%7D_U%29%2C~~~%5Ctext%7Bwhere%20%7D%20%5Chat%7B%5Ceta%7D_L%3D%5Chat%7B%5Ceta%7D%20-%20z%5E%7B%5Calpha%2F2%7D%20se%28%5Chat%7B%5Ceta%7D%29%2C~~~%20%5Chat%7B%5Ceta%7D_U%3D%5Chat%7B%5Ceta%7D%20%2B%20z%5E%7B%5Calpha%2F2%7D%20se%28%5Chat%7B%5Ceta%7D%29%2C%20~~~%20se%28%5Chat%7B%5Ceta%7D%29%3D%5Cfrac%7B1%7D%7Bn%7D%20%20x_0%20%5ET%20I%5E%7B-1%7D%28%5Chat%7B%5Cbeta%7D%20%29x_0 "(\hat{\eta}_L, \hat{\eta}_U),~~~\text{where } \hat{\eta}_L=\hat{\eta} - z^{\alpha/2} se(\hat{\eta}),~~~ \hat{\eta}_U=\hat{\eta} + z^{\alpha/2} se(\hat{\eta}), ~~~ se(\hat{\eta})=\frac{1}{n}  x_0 ^T I^{-1}(\hat{\beta} )x_0")

![\hat{p} = \frac{e^{\hat{\eta}}}{1+e^{\hat{\eta}}}](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D%20%3D%20%5Cfrac%7Be%5E%7B%5Chat%7B%5Ceta%7D%7D%7D%7B1%2Be%5E%7B%5Chat%7B%5Ceta%7D%7D%7D "\hat{p} = \frac{e^{\hat{\eta}}}{1+e^{\hat{\eta}}}")

![(1-\alpha)%](https://latex.codecogs.com/png.latex?%281-%5Calpha%29%25 "(1-\alpha)%")
CI for
![\hat{p}](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D "\hat{p}"):

![(\frac{e^{\hat{\eta}\_L}}{1+e^{\hat{\eta}\_L}},\frac{e^{\hat{\eta}\_U}}{1+e^{\hat{\eta}\_U}})](https://latex.codecogs.com/png.latex?%28%5Cfrac%7Be%5E%7B%5Chat%7B%5Ceta%7D_L%7D%7D%7B1%2Be%5E%7B%5Chat%7B%5Ceta%7D_L%7D%7D%2C%5Cfrac%7Be%5E%7B%5Chat%7B%5Ceta%7D_U%7D%7D%7B1%2Be%5E%7B%5Chat%7B%5Ceta%7D_U%7D%7D%29 "(\frac{e^{\hat{\eta}_L}}{1+e^{\hat{\eta}_L}},\frac{e^{\hat{\eta}_U}}{1+e^{\hat{\eta}_U}})")

``` r
x.s<-as.matrix(model.matrix(lmodr))
apply(x.s, 2, median)
```

    ##  (Intercept)          age       height          bmi          sdp         chol 
    ##       1.0000      45.0000      70.0000      24.3898     126.0000     223.0000 
    ##       dibepB         cigs arcuspresent 
    ##       0.0000       0.0000       0.0000

``` r
new_data <- data.frame(
  age = 45,
  height = 70,
  bmi = 24.4,
  sdp = 126,
  chol = 223,
  dibep = factor("A", levels = levels(wcgs$dibep)),
  cigs = 0,
  arcus = factor("absent", levels = levels(wcgs$arcus))
)

xb.pred<-predict(lmodr,new_data,se.fit =T) #predict x*beta, se

predict(lmodr,new_data,se.fit =T, type="response") #predict p, se
```

    ## $fit
    ##          1 
    ## 0.05108088 
    ## 
    ## $se.fit
    ##           1 
    ## 0.006569112 
    ## 
    ## $residual.scale
    ## [1] 1

``` r
(xb.pred.ci<-xb.pred$fit + c(-1,1)*1.96*xb.pred$se.fit)
```

    ## [1] -3.187542 -2.656284

``` r
(p.pred <- exp(xb.pred$fit)/(1+exp(xb.pred$fit)))
```

    ##          1 
    ## 0.05108088

``` r
exp(xb.pred.ci)/(1+exp(xb.pred.ci))
```

    ## [1] 0.03963724 0.06560273

``` r
predict(lmodr,new_data,se.fit =T, type="response")
```

    ## $fit
    ##          1 
    ## 0.05108088 
    ## 
    ## $se.fit
    ##           1 
    ## 0.006569112 
    ## 
    ## $residual.scale
    ## [1] 1

### Goodness of Fit (section 2.6)

#### Calibration

1.  Data Preparation and Ordering: The script begins by extracting the
    predicted probabilities
    (![\hat{p}\_1,...,\hat{p}\_n](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D_1%2C...%2C%5Chat%7Bp%7D_n "\hat{p}_1,...,\hat{p}_n"))
    and the actual binary outcomes
    (![y_1,...,y_n](https://latex.codecogs.com/png.latex?y_1%2C...%2Cy_n "y_1,...,y_n"))
    from the fitted logistic model lmodr. Both the probabilities and the
    corresponding outcomes are then sorted in ascending order based on
    the predicted probability values.

![\hat{p}\_{s1} \leq \hat{p}\_{s2} \leq ... \leq \hat{p}\_{sn}](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D_%7Bs1%7D%20%5Cleq%20%5Chat%7Bp%7D_%7Bs2%7D%20%5Cleq%20...%20%5Cleq%20%5Chat%7Bp%7D_%7Bsn%7D "\hat{p}_{s1} \leq \hat{p}_{s2} \leq ... \leq \hat{p}_{sn}")

![y\_{s1}, y\_{s2},..., y\_{sn}](https://latex.codecogs.com/png.latex?y_%7Bs1%7D%2C%20y_%7Bs2%7D%2C...%2C%20y_%7Bsn%7D "y_{s1}, y_{s2},..., y_{sn}")

2.  Grouping (e.g., partitioning into 10 Bins) Calculates the thresholds
    (10th, 20th, …, 100th percentiles) of the predicted probabilities.
    It then categorizes the sorted probabilities into 10 distinct groups
    (bins) using these quantiles as breakpoints. The predicted
    probabilities
    ![\hat{p}](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D "\hat{p}")
    and the corresponding observed outcome
    ![y](https://latex.codecogs.com/png.latex?y "y") in bin
    ![j](https://latex.codecogs.com/png.latex?j "j") are:

![(\hat{p}\_{ij},y\_{ij}),\~\~~ i=1,...,n_j,\~\~~ j=1,...,10](https://latex.codecogs.com/png.latex?%28%5Chat%7Bp%7D_%7Bij%7D%2Cy_%7Bij%7D%29%2C~~~%20i%3D1%2C...%2Cn_j%2C~~~%20j%3D1%2C...%2C10 "(\hat{p}_{ij},y_{ij}),~~~ i=1,...,n_j,~~~ j=1,...,10")

3.  Calculating Bin Statistics: For each of the 10 bins, the following
    aggregate metrics are computed:

- Mean Predicted Probability: The average predicted value within the bin

  ![\bar{\hat{p}}\_j = \frac{1}{n_j}\sum\_{i=1}^{n_j} \hat{p}\_{ij},\~\~~ j=1,...,10](https://latex.codecogs.com/png.latex?%5Cbar%7B%5Chat%7Bp%7D%7D_j%20%3D%20%5Cfrac%7B1%7D%7Bn_j%7D%5Csum_%7Bi%3D1%7D%5E%7Bn_j%7D%20%5Chat%7Bp%7D_%7Bij%7D%2C~~~%20j%3D1%2C...%2C10 "\bar{\hat{p}}_j = \frac{1}{n_j}\sum_{i=1}^{n_j} \hat{p}_{ij},~~~ j=1,...,10")
- Observed Proportion & sd : The actual fraction of positive events
  within the bin.

![\bar{y}\_j = \frac{1}{n_j}\sum\_{i=1}^{n_j} y\_{ij},\~\~~ j=1,...,10](https://latex.codecogs.com/png.latex?%5Cbar%7By%7D_j%20%3D%20%5Cfrac%7B1%7D%7Bn_j%7D%5Csum_%7Bi%3D1%7D%5E%7Bn_j%7D%20y_%7Bij%7D%2C~~~%20j%3D1%2C...%2C10 "\bar{y}_j = \frac{1}{n_j}\sum_{i=1}^{n_j} y_{ij},~~~ j=1,...,10")

![sd(\bar{y}\_j) =\sqrt{\frac{\bar{y}\_j (1-\bar{y}\_j)}{n_j}},\~\~~ j=1,...,10](https://latex.codecogs.com/png.latex?sd%28%5Cbar%7By%7D_j%29%20%3D%5Csqrt%7B%5Cfrac%7B%5Cbar%7By%7D_j%20%281-%5Cbar%7By%7D_j%29%7D%7Bn_j%7D%7D%2C~~~%20j%3D1%2C...%2C10 "sd(\bar{y}_j) =\sqrt{\frac{\bar{y}_j (1-\bar{y}_j)}{n_j}},~~~ j=1,...,10")

![95\\ \~~\text{CI}:\~\~~ \bar{y}\_j \pm 1.96 sd(\bar{y}\_j) ,\~\~~ j=1,...,10](https://latex.codecogs.com/png.latex?95%5C%25%20~~%5Ctext%7BCI%7D%3A~~~%20%5Cbar%7By%7D_j%20%5Cpm%201.96%20sd%28%5Cbar%7By%7D_j%29%20%2C~~~%20j%3D1%2C...%2C10 "95\% ~~\text{CI}:~~~ \bar{y}_j \pm 1.96 sd(\bar{y}_j) ,~~~ j=1,...,10")

``` r
# Data Preparation and Ordering
pred.prob <- predict(lmodr, type="response")
y<-  lmodr$y

pred.prob.o<-sort(pred.prob)
y.o<-y[order(pred.prob)]

# Grouping
(quantile.p<-quantile(pred.prob.o, (1:10)/10))
```

    ##        10%        20%        30%        40%        50%        60%        70% 
    ## 0.01806538 0.02614876 0.03429338 0.04458150 0.05731947 0.07158794 0.09100040 
    ##        80%        90%       100% 
    ## 0.11926466 0.17638819 0.91022455

``` r
group<-cut(pred.prob.o, breaks=c(0,quantile.p))

# Calculating Bin Statistics
x.c<-tapply(pred.prob.o, group ,mean) # mean predicted probability
y.c<-tapply(y.o, group, mean)        # observed proportion
bin.count<-tapply(y.o, group, length)# bin count:number of observations in each bin
y.sd <- sqrt(y.c*(1-y.c)/bin.count)  # sd(y_hat)
ci.u<-y.c+1.96*y.sd                  # CI
ci.l<-y.c-1.96*y.sd

plot(x.c,y.c,xlab="predicted probability",ylab="observed proportion",ylim=c(0,.3),xlim=c(0,.3))
abline(0,1)
apply(as.matrix(quantile.p),2,function(x) abline(v=x, lty=2))
```

    ## NULL

``` r
apply(cbind(x.c,ci.u,ci.l),1,function(x) segments(x[1],x[2],x[1],x[3],col=2))
```

![](w6-20260402-binary-response_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## NULL

#### Discrimination

Given a cutoff ![c](https://latex.codecogs.com/png.latex?c "c"), if
![\hat{p}\_i \> c~ (\leq c)](https://latex.codecogs.com/png.latex?%5Chat%7Bp%7D_i%20%3E%20c~%20%28%5Cleq%20c%29 "\hat{p}_i > c~ (\leq c)"),
let
![\hat{y} = 1(0)](https://latex.codecogs.com/png.latex?%5Chat%7By%7D%20%3D%201%280%29 "\hat{y} = 1(0)")

-Sensitivity:

![\frac{\\\\i: \hat{y}\_i = 1, y_i=1 \\}{\\\\i: y_i=1\\}](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5C%23%5C%7Bi%3A%20%5Chat%7By%7D_i%20%3D%201%2C%20y_i%3D1%20%5C%7D%7D%7B%5C%23%5C%7Bi%3A%20y_i%3D1%5C%7D%7D "\frac{\#\{i: \hat{y}_i = 1, y_i=1 \}}{\#\{i: y_i=1\}}")

-Specificity:

![\frac{\\\\i: \hat{y}\_i = 0, y_i=0 \\}{\\\\i: y_i=0\\}](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5C%23%5C%7Bi%3A%20%5Chat%7By%7D_i%20%3D%200%2C%20y_i%3D0%20%5C%7D%7D%7B%5C%23%5C%7Bi%3A%20y_i%3D0%5C%7D%7D "\frac{\#\{i: \hat{y}_i = 0, y_i=0 \}}{\#\{i: y_i=0\}}")

ROC curve:

- x axis: Sensitivity
- y axis: 1-Specificity

``` r
#install.packages("pROC")
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
#y<-  ifelse(wcgsm$chd == "no",0,1)

roc.lmod<-roc(y,pred.prob)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc.lmod)
```

    ## Area under the curve: 0.7553

``` r
plot(roc.lmod)
```

![](w6-20260402-binary-response_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Estimation Problem (Section 2.7)

We take a subset of the famous Fisher Iris data to consider only two of
the three species of Iris and use only two of the potential predictors:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
irisr <- filter(iris, Species != "virginica") %>%  select(Sepal.Width, Sepal.Length,Species)

irisr <- iris[iris$Species != "virginica",c(1,2,5)]

plot(irisr$Sepal.Width, irisr$Sepal.Length, col=irisr$Species)
lmod <- glm(Species ~ Sepal.Width + Sepal.Length, family=binomial, irisr)
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
tmp<-lmod$coefficients
x<-c(2,5)
y<-(-tmp[1]-x*tmp[2])/tmp[3]
points(x,y,type="l",col=3)
```

![](w6-20260402-binary-response_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Two Species of iris can be perfectly separated by sepal length and
width. We see that there were problems with the convergence.

``` r
lmod <- glm(Species ~ Sepal.Width + Sepal.Length, family=binomial, irisr)
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(lmod)
```

    ## 
    ## Call:
    ## glm(formula = Species ~ Sepal.Width + Sepal.Length, family = binomial, 
    ##     data = irisr)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)    -360.6   195972.9  -0.002    0.999
    ## Sepal.Width    -110.1    55361.5  -0.002    0.998
    ## Sepal.Length    131.8    64577.0   0.002    0.998
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1.3863e+02  on 99  degrees of freedom
    ## Residual deviance: 7.1185e-09  on 97  degrees of freedom
    ## AIC: 6
    ## 
    ## Number of Fisher Scoring iterations: 25

Notice that the residual deviance is zero, indicating a perfect fit
where the likelihood is equivalent to the saturated model. However, none
of the predictors are significant due to the high standard errors.

The line is computed by using the fitted model, we add the line
corresponding to
![p=.5](https://latex.codecogs.com/png.latex?p%3D.5 "p=.5")

For the data, we suffer from an “embarrassment of riches”. Whether the
data is effective depends on the purpose of analysis:

-For prediction or classification purpose: useful

-For parameter estimation: not so useful
