# balancedSurveyR

R package implementing a model-based sampling method that corrects for nonresponse bias in surveys.

## Installation

```{R}
devtools::install_github('brockf/balancedSurveyR')
```
  
## Example

```{R}
library('balancedSurveyR')
library('dplyr')
library('ggplot2')

# generate fake historical data
historical_responses <- expand.grid(
 person=1:100,
 age = 20:50
) %>%
 mutate(
   responded = rbinom(nrow(.), size=1, prob=(age / 100))
 ) %>%
 select(-starts_with('.')) %>%
 select(-person)

# fit historical_model()
model <- historical_model(data = historical_responses,
                         attribute_columns = c('age'),
                         response_column = 'responded',
                         model = 'xgbTree')

# verify predictions
historical_responses$predictions <- model$predictions
ggplot(historical_responses, aes(x=predictions, y=responded)) +
 stat_summary(fun.y='mean', geom='point') +
 stat_smooth(method="glm", method.args=list(family="binomial"))

# generate new population data
survey_population <- expand.grid(
 person=1:1000,
 age = 20:50
) %>%
 select(-person)

# generate sample
sampled_population <- sampler(population = survey_population,
                             model = model,
                             desired_n = 1000)

# examine characteristics
# sampled characteristics should be biased towards under-responders
sampled_characteristics <- characteristics(sample = sampled_population,
                                          population = survey_population,
                                          model = model,
                                          state = 'sampled')

sampled_characteristics

# responder characteristics should be representative of population
responder_characteristics <- characteristics(sample = sampled_population,
                                            population = survey_population,
                                            model = model,
                                            state = 'responded')

responder_characteristics
```

