## Questions
- Time as categorical or continuous ?!

## To dos
- Add categorical variable that is 1 iff dist > 50 to indicate that ch

## Notes
- male, female
- 50, 100, 200, 400
- freestyle, back, breast, butterfly, medley
- long, short

## Modeling

Available variables are _event_ ``e``, _distance_ ``d``, _stroke_ ``st``,
_sex_ ``se``, _course_ ``c``. The basic way that this model should work is

```y = x*beta +  alpha``` 

where ``x`` is the distance of the race, ``y`` is the total time the swimmer
needed to complete the course and ``beta`` is the average pace of the swimmer.
``alpha`` is the intercept, which is a time added to the total time that does not depend
on the distance in and by itself. For example, every racer might have some time before 
the starting gun and them jumping into the water.

Of course, the beta will depend on what kind of race we have, and what kind of gender etc.

Let's think in particular about how the ``course`` variable affects the total time of the swimmer.
Making a turn obviously adds time. Let's say that making one term takes `gamma` seconds of time. Thus, when 
the course is ``short``, we make twice as many turns (except for the 50m!) 

## Model Analysis

```
mod6 = lm(time ~ (dist + stroke + sex + n.splits)^3
    - dist:stroke:n.splits
    - dist:sex:n.splits
    - stroke:sex:n.splits, data = d)
    
mod6.extra <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d)
  
weighted_model <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d, weights = 1/d[, "time"])
  

weighted_model_sq <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d, weights = 1/(d$time*d$time))
```

When drawing the plots, this is what we see:

### Residuals vs. fitted values:
- The two shortest distances are estimated worse than the longer ones. So "we see a pattern" which
    means there might be an explanatory variable missing.
  - With the original model, we have AIC = 523, RSE = 1.746.
  - With adding ``I(dist^2)*stroke`` we get ``RSE = 1.648 and AIC = 472``
- **Variance of the residuals increases -> This is a problem**
  - However, we can try fixing this with a Box-Cox Transformation. And it also kind of makes sense:
        We can not extrapolate the average pace for an athlete based on (style, gender, course) and then apply
        this to any sort of distance. The Average pace will decrease the longer the race goes. So let's try that.
  - Box Cox showed that lambda = 1 is the most likely value
- The Standardised residuals increase as a function of the fitted value. However they should actually
    have normal distribution and be independent of y hat
  - Adding, as weights, the values 1/y, we get RSE = 0.1277 and AIC = -1804 (I checked the notes and negative AIC is ok)
  - Adding, as weights, the values 1/y^2, we get RSE =  0.01238 and AIC = -3886.433
  - From the notes, p. 28: "We often mistakenly fit a model of constant variance to data
    in which the variance of the response increases with the underlying mean. This model 
    misspecification is shown by a trend of increasing variance in r as y􏰑 increases."
    - The standardized residuals are still increasing if the weight is 1/y. Let's try 1/y^2.
    - That is really good. Now the variance does not look like 1, but at least the standardised residual
        is independent of y hat which is a good starting point. This is the model. Done.

### Normal Q-Q
- Pretty straight line but it is heavy tailed. Don't think we can do much about it because we need to use 
    a *normal* model .


## Outlier Detection

- From the lecture notes: 
  - Let `r_i` be the *standardized* residual. A rough rule of thumb is that `|r_i| > 2` 
        and `h_ii > 2p/n` are reasons for concern.
  - Points exceeding `C_k \approx 8/(n-2p)` have high influence.

- The model has 32 coefficients and 446 samples so `C_k = 0.02.` 
  From the plot we see we need to remove observations 280, 376, 424.
- Once we remove those errors we get that the RSE = 0.01209, which is down from 0.01238. 




