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
```

When drawing the plots, this is what we see:

### Residuals vs. fitted values:
- The shortest race distance is overestimated
- The next one is slightly overestimate
- **Variance of the residuals increases -> This is a problem**
  - However, we can try fixing this with a Box-Cox Transformation. And it also kind of makes sense:
        We can not extrapolate the average pace for an athlete based on (style, gender, course) and then apply
        this to any sort of distance. The Average pace will decrease the longer the race goes. So let's try that.
  - 

### Normal Q-Q
- Pretty straight line but it is heavy tailed. Don't think we can do much about it because we need to use 
    a *normal* model .

### Residuals vs. Leverage
- abc
- def

