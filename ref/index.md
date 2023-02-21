---
title: "Assessing interation in epidemiological studies"
author: "Ehsan Karim, ehsan.karim@ubc.ca, https://ehsank.com/"
date: "20 February 2023"
always_allow_html: yes
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    toc: yes
    number_sections: true
    toc_depth: 2
    toc_float: 
      collapsed: true
      smooth_scroll: true
    theme: lumen
    highlight: textmate
  pdf_document:
    fig_caption: yes
    latex_engine: xelatex
    number_sections: yes
    toc: yes
    toc_depth: 3
  word_document:
    toc: yes
  slidy_presentation:
    toc: yes  
bibliography: ref.bib       
tags:
- interaction
- R
- epidemiology
header-includes: 
- \usepackage[utf8]{inputenc}
- \usepackage{doi}
- \usepackage{url}
- \usepackage{hyperref} 
- \usepackage{apacite} 
- \usepackage[usedoi]{rsc} % Loads natbib
- \setcitestyle{sort&compress,numbers,super,square}
---



# Effect modification vs interaction

Useful references 

- @vanderweele2009distinction,
- @vanderweele2011interpretation,
- @rothman2012epidemiology, 
- @vanderweele2014tutorial,
- @bours2021tutorial, 
- @whitcomb2023interaction 

## Effect modification

Causal effect of exposure (A) on outcome (Y) depends upon levels of a third factor (B)

## Interaction

Causal effect of combination of multiples exposures (A and B) on outcome (Y)

# DAG

<div class="figure">
<img src="images/dag.png" alt="An illustration of possible interaction by  while investigating the impact of two dichotomous factors: $A$ (alcohol [**alc**]) and $B$ (tobacco smoking [**smk**]) on the dichotomous outcome $Y$ (oral cancer [**oc**]).\label{fig:dag}" width="100%" />
<p class="caption">An illustration of possible interaction by  while investigating the impact of two dichotomous factors: $A$ (alcohol [**alc**]) and $B$ (tobacco smoking [**smk**]) on the dichotomous outcome $Y$ (oral cancer [**oc**]).\label{fig:dag}</p>
</div>


# Example data

**Data source**: @rothman1972effect


```r
require(interactionR)
data(OCdata)
dim(OCdata)
```

```
## [1] 458   3
```

```r
summary(OCdata)
```

```
##        oc              alc             smk        
##  Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:1.0000  
##  Median :1.0000   Median :1.000   Median :1.0000  
##  Mean   :0.5284   Mean   :0.893   Mean   :0.9105  
##  3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.000   Max.   :1.0000
```

Variables

- **oc**, oral cancer, outcome (Y)
- **alc**, alcohol use, first exposure (A)
- **smk**, smoking, second exposure (B)


# Assessing interaction and modelling

We will be using three measures of effect to explain the concept and necessary implementation ideas


# Modelling interaction to obtain OR

 - odds ratio (OR),  Multiplicative scale
    - Logit[Pr(Y=1)] = $\alpha_0$ + $\alpha_A$ A + $\alpha_B$ B + $\alpha_{AB}$ (A $\times$ B)
    - Condition for presence of interaction ("departure from multiplicativity")
        - OR10 $\times$ OR01 $\ne$ OR11
        - $\exp(\alpha_A)$ $\times$ $\exp(\alpha_B)$ $\ne$ $\exp(\alpha_A+ \alpha_B + \alpha_{AB})$
    - synergism / super-multiplicative
        - OR11 > OR10 $\times$ OR01
    - antagonism / sub-multiplicative
        - OR11 < OR10 $\times$ OR01
        
*Note*: 
The above assumes that both exposures are risk factors for the outcome (OR > 1), not protective factors. If protective, estimates of RERI and AP (defined later) will be invalid, although the estimate of SI is not affected by this condition.

## Base 

### no alcohol, no smoking for OR(smk1 on outcome [alc1==0] and OR(alc1 on outcome [smk1==0]


```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "0")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "0")
fit.or.int11 <- glm(oc ~ alc + smk + alc:smk, family = binomial(link = 'logit'), data = Obs.Data)
require(jtools)
results.or.model11 <- summ(fit.or.int11, model.info = FALSE, model.fit = FALSE, exp = TRUE, robust = "HC1", confint = TRUE)
results.or.model11
```

```
## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
## %in% : 'length(x) = 4 > 1' in coercion to 'logical(1)'
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> -3.06 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 3.33 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 15.86 </td>
   <td style="text-align:right;"> 1.51 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 2.96 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> 12.91 </td>
   <td style="text-align:right;"> 1.45 </td>
   <td style="text-align:right;"> 0.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk1 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 5.42 </td>
   <td style="text-align:right;"> -0.10 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>

## Estimates of different ORs

<!----
### Base 

- $\alpha_0$ = -1.9
- $\exp(\alpha_0)$ = 0.15
--->


### Alcohol


```r
# OR00 = 1
OR10 <- exp(sum(summary(fit.or.int11)$coef[c('alc1'),'Estimate']))
OR10 # OR_A=1
```

```
## [1] 3.333333
```

- $\alpha_A$ = 1.2
- OR_{A=1} = OR10 = $\exp(\alpha_A)$ = 3.33

### Smoking


```r
OR01 <- exp(sum(summary(fit.or.int11)$coef[c('smk1'),'Estimate']))
OR01 # OR_B=1
```

```
## [1] 2.962963
```

- $\alpha_B$ = 1.09
- OR_{B=1} = OR01 = $\exp(\alpha_B)$ = 2.96

### Both alcohol and smoking


```r
OR11 <- exp(sum(summary(fit.or.int11)$coef[c('alc1','smk1','alc1:smk1'),'Estimate'])) 
OR11 #OR_A=1,B=1
```

```
## [1] 9.036145
```

- $\alpha_{AB}$ = -0.09
- OR_{A=1,B=1} = OR11 = 9.04

### Joint effect?

Is OR11 $\ne$ OR10 * OR01


```r
OR10 * OR01
```

```
## [1] 9.876543
```

### Additive scales

- Relative excess risk due to interaction (RERI)
  - ranges from 0 to $\infty$
  - 0 means no interaction
  - +ve means positive interaction 
- Proportion of outcome among those with both exposures that is attributable to the interaction (AP)
  - ranges from -1 to 1
  - 0 means no interaction 
- Synergy index (SI) 
  - ratio of the combined effects and the individual effects
  - ranges from 0 to $\infty$
  - greater than 1 means positive interaction



```r
RERI <- OR11 - OR10 - OR01 + 1
RERI
```

```
## [1] 3.739848
```

```r
AP <- RERI / OR11
AP
```

```
## [1] 0.4138765
```

```r
SI <- (OR11 - 1)/ (OR10 - 1 + OR01 - 1)
SI
```

```
## [1] 1.870482
```

### Recoding to obtain different combinations

#### Base: alcohol drinker, no smoking for OR(smk1 on outcome [alc1==1]


```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "0")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "1")
fit.or.int01 <- glm(oc ~ alc + smk + alc:smk, family = binomial(link = 'logit'), data = Obs.Data)
results.or.model01 <- summ(fit.or.int01, model.info = FALSE, model.fit = FALSE, exp = TRUE, robust = "HC1", confint = TRUE)
results.or.model01
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.33 </td>
   <td style="text-align:right;"> -1.39 </td>
   <td style="text-align:right;"> 0.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc0 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 1.43 </td>
   <td style="text-align:right;"> -1.51 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 2.71 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 7.37 </td>
   <td style="text-align:right;"> 1.95 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc0:smk1 </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 6.48 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>

#### Base: Smoker, no alcohol for OR(alc1 on outcome [smk1==1]


```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "1")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "0")
fit.or.int10 <- glm(oc ~ alc + smk + alc:smk, family = binomial(link = 'logit'), data = Obs.Data)
results.or.model10 <- summ(fit.or.int10, model.info = FALSE, model.fit = FALSE, exp = TRUE, robust = "HC1", confint = TRUE)
results.or.model10
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:right;"> -1.91 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 3.05 </td>
   <td style="text-align:right;"> 1.29 </td>
   <td style="text-align:right;"> 7.18 </td>
   <td style="text-align:right;"> 2.55 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk0 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 1.47 </td>
   <td style="text-align:right;"> -1.45 </td>
   <td style="text-align:right;"> 0.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk0 </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 6.48 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>

```r
OR10b <- exp(sum(summary(fit.or.int10)$coef[c('alc1'),'Estimate']))
OR10b
```

```
## [1] 3.049699
```

### Multiplicative scale


```r
Multiplicative.scale <- exp(sum(summary(fit.or.int11)$coef[c('alc1:smk1'),'Estimate'])) 
Multiplicative.scale
```

```
## [1] 0.9149096
```

- Multiplicative.scale = $\exp(\alpha_{AB})$ = 0.91


```r
OR10b / OR10
```

```
## [1] 0.9149096
```

## Reporting guideline

Ref: @knol2012recommendations, @alli2021interactionr


```r
int.object <- interactionR(fit.or.int11, 
                            exposure_names = c("alc1", "smk1"), 
                            ci.type = "mover", ci.level = 0.95, 
                            em=FALSE, recode = FALSE)
kable(int.object$dframe[,1:4], digits = 2)
```



|Measures                     | Estimates|  CI.ll| CI.ul|
|:----------------------------|---------:|------:|-----:|
|OR00                         |      1.00|     NA|    NA|
|OR01                         |      2.96|   0.68| 12.91|
|OR10                         |      3.33|   0.70| 15.86|
|OR11                         |      9.04|   2.64| 30.91|
|OR(smk1 on outcome [alc1==0] |      2.96|   0.68| 12.91|
|OR(smk1 on outcome [alc1==1] |      2.71|   1.00|  7.37|
|OR(alc1 on outcome [smk1==0] |      3.33|   0.70| 15.86|
|OR(alc1 on outcome [smk1==1] |      3.05|   1.29|  7.18|
|Multiplicative scale         |      0.91|   0.15|  5.42|
|RERI                         |      3.74| -11.43| 21.87|
|AP                           |      0.41|  -0.38|  0.81|
|SI                           |      1.87|   0.65|  5.42|

## Interaction definitions for OR

| Groups and Conditions 	| Risk 	| Odds Ratio |
|---	|---	|---	|
| Baseline (exposed to none) 	| $R_{A = 0, B = 0} =  P[Y^{a=0, b=0} | L]$ 	| Reference | 
| Exposed to $A$ only 	| $R_{A = 1, B = 0} =  P[Y^{a=1, b=0} | L]$ 	| $OR_{A = 1} = [R_{A = 1, B = 0} / (1-R_{A = 1, B = 0})] / [R_{A = 0, B = 0} / (1 - R_{A = 0, B = 0})]$ |
| Exposed to $B$ only 	| $R_{A = 0, B = 1} =  P[Y^{a=0, b=1} | L]$ 	| $OR_{B = 1} = [R_{A = 0, B = 1} / (1-R_{A = 0, B = 1})] / [R_{A = 0, B = 0} / (1 - R_{A = 0, B = 0})]$ |
| Exposed to both $A$ and $B$ 	| $R_{A = 1, B = 1} =  P[Y^{a=1, b=1} | L]$ 	| $OR_{A = 1, B = 1} = [R_{A = 1, B = 1} / (1-R_{A = 1, B = 1})] / [R_{A = 0, B = 0} / (1 - R_{A = 0, B = 0})]$ |
| **Condition for interaction** 	 	|  | $OR_{A = 1, B = 1} \ne OR_{A = 1} \times OR_{B = 1}$	|
| **Synergism** 	 	||   $OR_{A = 1, B = 1} > OR_{A = 1} \times OR_{B = 1}$	|
| **Antagonism** 	 	||  $OR_{A = 1, B = 1} < OR_{A = 1} \times OR_{B = 1}$	|
Table: \label{tab:def} Summary of interaction definition for OR.


# Modelling interaction to obtain RR

 - risk ratio (RR), Multiplicative scale
    - Very similar ideas as in OR
    

```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "0")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "0")
fit.rr.int11 <- glm(oc ~ alc + smk + alc:smk, family = poisson(link = 'log'), data = Obs.Data)
results.rr.model11 <- summ(fit.rr.int11, model.info = FALSE, model.fit = FALSE, exp = TRUE, robust = "HC1", confint = TRUE)
results.rr.model11
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> -3.53 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 2.56 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 10.22 </td>
   <td style="text-align:right;"> 1.33 </td>
   <td style="text-align:right;"> 0.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 2.36 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 8.89 </td>
   <td style="text-align:right;"> 1.27 </td>
   <td style="text-align:right;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk1 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 3.46 </td>
   <td style="text-align:right;"> -0.39 </td>
   <td style="text-align:right;"> 0.69 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>

Ref: @naimi2020estimating

Alternate formulation using log-binomial is possible (using model based SE, but confidence intervals somewhat differ from those reported from poisson(log)):


```r
fit.rr.int11lb <- glm(oc ~ alc + smk + alc:smk, family = binomial(link = 'log'), data = Obs.Data)
results.rr.model11lb <- summ(fit.rr.int11lb, model.info = FALSE, model.fit = FALSE, exp = TRUE, confint = TRUE)
results.rr.model11lb
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> -3.78 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 2.56 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 8.84 </td>
   <td style="text-align:right;"> 1.48 </td>
   <td style="text-align:right;"> 0.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 2.36 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 7.85 </td>
   <td style="text-align:right;"> 1.40 </td>
   <td style="text-align:right;"> 0.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk1 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 2.88 </td>
   <td style="text-align:right;"> -0.45 </td>
   <td style="text-align:right;"> 0.66 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

## Estimates of different RRs

### Alcohol


```r
# RR00 = 1
RR10 <- exp(sum(summary(fit.rr.int11)$coef[c('alc1'),'Estimate']))
RR10 # RR_A=1
```

```
## [1] 2.555555
```

- $\alpha_A$ = 0.94
- RR_{A=1} = RR10 = $\exp(\alpha_A)$ = 2.56

### Smoking


```r
RR01 <- exp(sum(summary(fit.rr.int11)$coef[c('smk1'),'Estimate']))
RR01 # RR_B=1
```

```
## [1] 2.358974
```

- $\alpha_B$ = 0.86
- RR_{B=1} = RR01 = $\exp(\alpha_B)$ = 2.36

### Both alcohol and smoking


```r
RR11 <- exp(sum(summary(fit.rr.int11)$coef[c('alc1','smk1','alc1:smk1'),'Estimate'])) 
RR11 #RR_A=1,B=1
```

```
## [1] 4.411764
```

- $\alpha_{AB}$ = -0.31
- RR_{A=1,B=1} = RR11 = 4.41

### Joint effect?

Is RR11 $\ne$ RR10 * RR01


```r
RR10 * RR01
```

```
## [1] 6.028489
```

### Multiplicative scale


```r
Multiplicative.scale <- exp(sum(summary(fit.rr.int11)$coef[c('alc1:smk1'),'Estimate'])) 
Multiplicative.scale
```

```
## [1] 0.7318192
```

- Multiplicative.scale = $\exp(\alpha_{AB})$ = 0.73

### Additive scales


```r
RERI <- RR11 - RR10 - RR01 + 1
RERI
```

```
## [1] 0.4972348
```

```r
AP <- RERI / RR11
AP
```

```
## [1] 0.1127066
```

```r
SI <- (RR11 - 1)/ (RR10 - 1 + RR01 - 1)
SI
```

```
## [1] 1.170606
```


## Reporting guideline


```r
int.object <- interactionR(fit.rr.int11, 
                            exposure_names = c("alc1", "smk1"), 
                            ci.type = "mover", ci.level = 0.95, 
                            em=FALSE, recode = FALSE)
kable(int.object$dframe[,1:4], digits = 2)
```



|Measures                     | Estimates| CI.ll| CI.ul|
|:----------------------------|---------:|-----:|-----:|
|OR00                         |      1.00|    NA|    NA|
|OR01                         |      2.36|  0.63|  8.89|
|OR10                         |      2.56|  0.64| 10.22|
|OR11                         |      4.41|  1.41| 13.78|
|OR(smk1 on outcome [alc1==0] |      2.36|  0.63|  8.89|
|OR(smk1 on outcome [alc1==1] |      1.73|  0.77|  3.88|
|OR(alc1 on outcome [smk1==0] |      2.56|  0.64| 10.22|
|OR(alc1 on outcome [smk1==1] |      1.87|  0.92|  3.79|
|Multiplicative scale         |      0.73|  0.15|  3.46|
|RERI                         |      0.50| -9.97|  7.01|
|AP                           |      0.11| -0.82|  0.75|
|SI                           |      1.17|  0.42|  3.23|

Ref: @alli2021interactionr

## Interaction definitions for RR

| Groups and Conditions 	| Risk 	| Risk Ratio | 
|---	|---	|---	|---	| ---	|
| Baseline (exposed to none) 	| $R_{A = 0, B = 0} =  P[Y^{a=0, b=0} | L]$ 	| Reference	|   
| Exposed to $A$ only 	| $R_{A = 1, B = 0} =  P[Y^{a=1, b=0} | L]$ 	|  $RR_{A = 1} = R_{A = 1, B = 0} / R_{A = 0, B = 0}$	| 
| Exposed to $B$ only 	| $R_{A = 0, B = 1} =  P[Y^{a=0, b=1} | L]$ 	|  $RR_{B = 1} = R_{A = 0, B = 1} / R_{A = 0, B = 0}$	| 
| Exposed to both $A$ and $B$ 	| $R_{A = 1, B = 1} =  P[Y^{a=1, b=1} | L]$ 	|  $RR_{A = 1, B = 1} = R_{A = 1, B = 1} / R_{A = 0, B = 0}$	| 
| **Condition for interaction** 	 	||  $RR_{A = 1, B = 1} \ne RR_{A = 1} \times RR_{B = 1}$ 	
| **Synergism** 	 	||   $RR_{A = 1, B = 1} > RR_{A = 1} \times RR_{B = 1}$ 	|
| **Antagonism** 	 	||   $RR_{A = 1, B = 1} < RR_{A = 1} \times RR_{B = 1}$ 	|
Table: \label{tab:def} Summary of interaction definition for RR.


# Modelling interaction to obtain RD

 - risk difference (RD), Additive scale
    - risk(Y) = $\alpha_0$ + $\alpha_A$ A + $\alpha_B$ B + $\alpha_{AB}$ (A $\times$ B)
        - RD10 $\times$ RD01 $\ne$ RD11
        - $\alpha_A$ + $\alpha_B$ $\ne$ ($\alpha_A$ + $\alpha_B$ + $\alpha_{AB}$)
    - synergism / super-additive
        - RD11 > RD10 + RD01
    - antagonism / sub-additive
        - RD11 < RD10 + RD01    



```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "0")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "0")
fit.rd.int11 <- glm(oc ~ alc + smk + alc:smk, family = gaussian(link = 'identity'), data = Obs.Data)
results.rd.model11 <- summ(fit.rd.int11, model.info = FALSE, model.fit = FALSE, exp = FALSE, robust = "HC1", confint = TRUE)
results.rd.model11
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Est. </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> t val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> 0.33 </td>
   <td style="text-align:right;"> 1.28 </td>
   <td style="text-align:right;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> -0.10 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 1.32 </td>
   <td style="text-align:right;"> 0.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.10 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 1.27 </td>
   <td style="text-align:right;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk1 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> -0.29 </td>
   <td style="text-align:right;"> 0.42 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.72 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>

Ref: @naimi2020estimating


Alternate formulation using binomial(identity) is possible (using model based SE, but confidence intervals slightly differ from those reported from gaussian(identity)):


```r
fit.rd.int11bi <- glm(oc ~ alc + smk + alc:smk, family = binomial(link = 'identity'), data = Obs.Data)
results.rd.model11bi <- summ(fit.rd.int11bi, model.info = FALSE, model.fit = FALSE, exp = FALSE, confint = TRUE)
results.rd.model11bi
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Est. </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> 1.86 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 1.54 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk1 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 1.55 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk1 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> -0.25 </td>
   <td style="text-align:right;"> 0.38 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 0.69 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE</td></tr></tfoot>
</table>

## Base: Smoker, no alcohol for RD(alc1 on outcome [smk1==1]
Alternate combination of smoking:


```r
Obs.Data <- OCdata
Obs.Data$smk <- as.factor(Obs.Data$smk)
Obs.Data$smk <- relevel(Obs.Data$smk, ref = "1")
Obs.Data$alc <- as.factor(Obs.Data$alc)
Obs.Data$alc <- relevel(Obs.Data$alc, ref = "0")
fit.rd.int01 <- glm(oc ~ alc + smk + alc:smk, family = gaussian(link = 'identity'), data = Obs.Data)
results.rd.model01 <- summ(fit.rd.int01, model.info = FALSE, model.fit = FALSE, exp = FALSE, robust = "HC1", confint = TRUE)
results.rd.model01
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Est. </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> t val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 3.22 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 2.71 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smk0 </td>
   <td style="text-align:right;"> -0.18 </td>
   <td style="text-align:right;"> -0.45 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> -1.27 </td>
   <td style="text-align:right;"> 0.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> alc1:smk0 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> -0.42 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> -0.36 </td>
   <td style="text-align:right;"> 0.72 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC1</td></tr></tfoot>
</table>


## Estimates of different RDs

### Alcohol


```r
# RD00 = 0
RD10 <- sum(summary(fit.rd.int11)$coef[c('alc1'),'Estimate'])
RD10 # RD_A=1
```

```
## [1] 0.2028986
```

- RD_{A=1} = RD10 = $\alpha_A$ = 0.2

From alternate combination of smoking


```r
RD10b <- sum(summary(fit.rd.int01)$coef[c('alc1'),'Estimate'])
RD10b # RD_A=1
```

```
## [1] 0.2677553
```


### Smoking


```r
RD01 <- sum(summary(fit.rd.int11)$coef[c('smk1'),'Estimate'])
RD01 # RD_B=1
```

```
## [1] 0.1772575
```

- RD_{B=1} = RD01 = $\alpha_B$ = 0.18


### Both alcohol and smoking


```r
RD11 <- sum(summary(fit.rd.int11)$coef[c('alc1','smk1','alc1:smk1'),'Estimate'])
RD11 #RD_A=1,B=1
```

```
## [1] 0.4450128
```

- RD_{A=1,B=1} = RD11 = $\alpha_{AB}$ = 0.06

### Joint effect?

Is RD11 $\ne$ RD10 + RD01


```r
RD10 + RD01
```

```
## [1] 0.3801561
```

### Additive scale (interaction contrast)


```r
IC <- RD10b - RD10
IC
```

```
## [1] 0.06485671
```

Current version of `interactionR` do not support RD, although the documentation says so: @alli2021interactionr

## Interaction definitions for RD

| Groups and Conditions 	| Risk 	| Risk Difference 	| 
|---	|---	|---	|---	| ---	|
| Baseline (exposed to none) 	| $R_{A = 0, B = 0} =  P[Y^{a=0, b=0} | L]$ 	| Reference	|  
| Exposed to $A$ only 	| $R_{A = 1, B = 0} =  P[Y^{a=1, b=0} | L]$ 	| $RD_{A = 1} = R_{A = 1, B = 0} - R_{A = 0, B = 0}$	| 
| Exposed to $B$ only 	| $R_{A = 0, B = 1} =  P[Y^{a=0, b=1} | L]$ 	| $RD_{B = 1} = R_{A = 0, B = 1} - R_{A = 0, B = 0}$	| 
| Exposed to both $A$ and $B$ 	| $R_{A = 1, B = 1} =  P[Y^{a=1, b=1} | L]$ 	| $RD_{A = 1, B = 1} = R_{A = 1, B = 1} - R_{A = 0, B = 0}$	| 
| **Condition for interaction** 	 	||  $RD_{A = 1, B = 1} \ne RD_{A = 1} + RD_{B = 1}$	| 
| **Synergism** 	 	||  $RD_{A = 1, B = 1} > RD_{A = 1} + RD_{B = 1}$	| 
| **Antagonism** 	 	||  $RD_{A = 1, B = 1} < RD_{A = 1} + RD_{B = 1}$	| 
Table: \label{tab:def} Summary of interaction definition for RD.


# Reporting guideline for interaction assessment

- How to determine whether interaction should be reported in additive or multiplicative scale?
  - Not via empirical assessment (analysis of data at hand)
  - Study objectives should be clear
  - data generating mechanism should be considered carefully
- What if results differ?
  - Very possible to have additive interaction present, but not at a multiplicative scale and vice versa
  - When conclusions differ (interaction is present in additive, but absent in multiplicative or vice versa), both should be reported.
  - When both are concluding the same (interaction is present in both scales or absent), questions related to magnitude remains

# Bibliography
