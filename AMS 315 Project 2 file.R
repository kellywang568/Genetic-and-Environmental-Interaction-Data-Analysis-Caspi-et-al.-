#Project 2, AMS 315
#First title the file given to me
Dat <- read.csv('P2_37466.csv', header=TRUE)
#Fit a model with only environmental variables
M_E <- lm(Y ~ E1+E2+E3+E4, data=Dat)
print(summary(M_E))
##Cal:
##lm(formula = Y ~ E1 + E2 + E3 + E4, data = Dat)
##
##Residuals:
##  Min        1Q    Median        3Q       Max 
##-11993182  -3528162   -508869   2802659  23941989 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) -16529068    1354085 -12.207   <2e-16 ***
##  E1             -28851      80244  -0.360    0.719    
##  E2            1677173      80351  20.873   <2e-16 ***
##  E3            1922821      78213  24.584   <2e-16 ***
##  E4             -13708      82188  -0.167    0.868    
##---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Residual standard error: 5268000 on 1075 degrees of freedom
##Multiple R-squared:  0.4981,	Adjusted R-squared:  0.4963 
##F-statistic: 266.7 on 4 and 1075 DF,  p-value: < 2.2e-16

#Find the adjusted R squared value
print(summary(M_E)$adj.r.squared)
##[1] 0.4962557

#Now we control environmental variables and assess for the contribution of genetic variables 
paste(paste0('E', 1:5), collapse = '+')
M_raw <- lm( Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20), data = Dat )
#We will create and examine the residual plot
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')
library(MASS)
boxcox(M_raw)
#Because my estimated lambda value appears to be around 0.5, I use Y^0.5
M_trans <- lm( I(Y^0.5) ~ (.), data=Dat )
print(summary(M_raw)$adj.r.square)
##[1] 0.5039505
#Because my r-squared value increased, this shows that the transformation created a more optimal graph 
print(summary(M_trans)$adj.r.square)
##[1] 0.5385358
#Plot the transformed graph now
#plot the residual graph now 
plot(resid(M_trans) ~fitted(M_trans), main='New Residual Plot')

#I made sure to install the package for leaps
#install.packages("leaps")
library(leaps)
M <- regsubsets( model.matrix(M_trans)[,-1], I((Dat$Y)^(1/2)),
                 nbest = 1 , nvmax=5, 
                 method = 'forward', intercept = TRUE )
temp <- summary(M)
#print(temp)
##Subset selection object
#install.packages("knitr")
#install.packages("kableExtra")
library(kableExtra)
library(knitr)
Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1, 
                  function(x) paste0(Var[x], collapse='+'))
print(kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
            caption='Model Summary'))
##<table>
##  <caption>Model Summary</caption>
##  <thead>
##  <tr>
##  <th style="text-align:left;"> model </th>
##  <th style="text-align:left;"> adjR2 </th>
##  <th style="text-align:left;"> BIC </th>
##  </tr>
##  </thead>
##  <tbody>
##  <tr>
##  <td style="text-align:left;"> (Intercept)+E3 </td>
##  <td style="text-align:left;"> 0.310041050142424 </td>
##  <td style="text-align:left;"> -387.844988462386 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> (Intercept)+E2+E3 </td>
##  <td style="text-align:left;"> 0.529894771830186 </td>
##  <td style="text-align:left;"> -796.232179373285 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> (Intercept)+E2+E3+G17 </td>
##  <td style="text-align:left;"> 0.539809220740668 </td>
##  <td style="text-align:left;"> -813.27136621407 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> (Intercept)+E2+E3+G3+G17 </td>
##  <td style="text-align:left;"> 0.541049665407465 </td>
##  <td style="text-align:left;"> -810.205905611854 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> (Intercept)+E2+E3+G3+G13+G17 </td>
##  <td style="text-align:left;"> 0.541495423885762 </td>
##  <td style="text-align:left;"> -805.275774736494 </td>
##  </tr>
##  </tbody>
##  </table>
M_main <- lm( I((Y)^(1/2)) ~ ., data=Dat)
temp <- summary(M_main)
print(kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients'))
##<table>
##  <caption>Sig Coefficients</caption>
##  <thead>
##  <tr>
##  <th style="text-align:left;">   </th>
##  <th style="text-align:right;"> Estimate </th>
##  <th style="text-align:right;"> Std. Error </th>
##  <th style="text-align:right;"> t value </th>
##  <th style="text-align:right;"> Pr(&gt;|t|) </th>
##  </tr>
##  </thead>
##  <tbody>
##  <tr>
##  <td style="text-align:left;"> E2 </td>
##  <td style="text-align:right;"> 236.8734 </td>
##  <td style="text-align:right;"> 10.59716 </td>
##  <td style="text-align:right;"> 22.352532 </td>
##  <td style="text-align:right;"> 0.0e+00 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> E3 </td>
##  <td style="text-align:right;"> 266.6243 </td>
##  <td style="text-align:right;"> 10.33195 </td>
##  <td style="text-align:right;"> 25.805792 </td>
##  <td style="text-align:right;"> 0.0e+00 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> G17 </td>
##  <td style="text-align:right;"> 248.2914 </td>
##  <td style="text-align:right;"> 53.31335 </td>
##  <td style="text-align:right;"> 4.657208 </td>
##  <td style="text-align:right;"> 3.6e-06 </td>
##  </tr>
##  </tbody>
##  </table>
M_2nd <- lm( I((Y)^(1/2)) ~ (.)^2, data=Dat)
temp  <- summary(M_2nd)
print(kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.01, ], caption='2nd Interaction'))
##  <table>
##  <caption>2nd Interaction</caption>
##  <thead>
##  <tr>
##  <th style="text-align:left;">   </th>
##  <th style="text-align:right;"> Estimate </th>
##  <th style="text-align:right;"> Std. Error </th>
##  <th style="text-align:right;"> t value </th>
##  <th style="text-align:right;"> Pr(&gt;|t|) </th>
##  </tr>
##  </thead>
##  <tbody>
##  <tr>
##  <td style="text-align:left;"> E2 </td>
##  <td style="text-align:right;"> 450.22196 </td>
##  <td style="text-align:right;"> 153.1956 </td>
##  <td style="text-align:right;"> 2.938871 </td>
##  <td style="text-align:right;"> 0.0033914 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> E3:G2 </td>
##  <td style="text-align:right;"> 84.37606 </td>
##  <td style="text-align:right;"> 31.7837 </td>
##  <td style="text-align:right;"> 2.654696 </td>
##  <td style="text-align:right;"> 0.0081000 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> G12:G20 </td>
##  <td style="text-align:right;"> 459.65138 </td>
##  <td style="text-align:right;"> 171.8968 </td>
##  <td style="text-align:right;"> 2.673997 </td>
##  <td style="text-align:right;"> 0.0076524 </td>
##  </tr>
##  </tbody>
##  </table>
M_2stage <- lm( I((Y)^(1/2)) ~ (E2+E3+G17), data=Dat)
temp <- summary(M_2stage)
print(kable(temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]))
##<table>
##  <thead>
##  <tr>
##  <th style="text-align:left;">   </th>
##  <th style="text-align:right;"> Estimate </th>
##  <th style="text-align:right;"> Std. Error </th>
##  <th style="text-align:right;"> t value </th>
##  <th style="text-align:right;"> Pr(&gt;|t|) </th>
##  </tr>
##  </thead>
##  <tbody>
##  <tr>
##  <td style="text-align:left;"> (Intercept) </td>
##  <td style="text-align:right;"> -942.8576 </td>
##  <td style="text-align:right;"> 130.10305 </td>
##  <td style="text-align:right;"> -7.247006 </td>
##  <td style="text-align:right;"> 0e+00 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> E2 </td>
##  <td style="text-align:right;"> 236.4378 </td>
##  <td style="text-align:right;"> 10.48283 </td>
##  <td style="text-align:right;"> 22.554760 </td>
##  <td style="text-align:right;"> 0e+00 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> E3 </td>
##  <td style="text-align:right;"> 268.9584 </td>
##  <td style="text-align:right;"> 10.18812 </td>
##  <td style="text-align:right;"> 26.399204 </td>
##  <td style="text-align:right;"> 0e+00 </td>
##  </tr>
##  <tr>
##  <td style="text-align:left;"> G17 </td>
##  <td style="text-align:right;"> 259.0149 </td>
##  <td style="text-align:right;"> 52.64888 </td>
##  <td style="text-align:right;"> 4.919667 </td>
##  <td style="text-align:right;"> 1e-06 </td>
##  </tr>
##  </tbody>
##  </table>
#We would use the above values to generate our final function for our picked model 