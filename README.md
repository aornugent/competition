# competition

Packaged dataset from response-surface experiment to evaluate whether the interspecific interactions (hereafter impact) of invasive species varied with nutrient availability and species density  (2016).

## Yield density curves

``` r
# load data
data(samples)
str(samples)

# format data
data_list <- format_data()
str(data_list)


mod <- stan("models/yield_density.stan",
            data = data_list,
            iter = 1000, chains = 3)

save(mod, file = "models/yield_density.Rdata")
```

