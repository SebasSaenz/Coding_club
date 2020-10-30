Simple Barplot for taxonomy
================
Sebastián Sáenz
30/10/2020

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- read.table("mock_tax.txt",
                 header = TRUE,
                 sep = "\t",
                 check.names = FALSE)

colSums(df[3:8])
```

    ##      Control    Control.1    Control.2   Antibiotic Antibiotic.1 Antibiotic.2 
    ##          100          100          100          100          100          100

``` r
df_long <- pivot_longer(df, cols = c(3:4),
             names_to = "Treatment",
             values_to ="Abundance")


relative_abundace <- df_long %>%
    group_by(Family, Treatment) %>%
    summarise(relative_abundance = sum(Abundance)/3)
```

    ## `summarise()` regrouping output by 'Family' (override with `.groups` argument)

``` r
sort.class <- relative_abundace %>% 
  count(Family, wt = relative_abundance) %>%
  arrange(desc(n)) %>%
  pull(Family) 

relative_abundace %>%
count(Family, wt =relative_abundance)
```

    ## # A tibble: 8 x 2
    ## # Groups:   Family [8]
    ##   Family                 n
    ##   <fct>              <dbl>
    ## 1 Bacteroidaceae     20.3 
    ## 2 Bifidobacteriaceae 28.3 
    ## 3 Clostridiaceae     10.1 
    ## 4 Helicobacteraceae   9.31
    ## 5 Lactobacillaceae   29.7 
    ## 6 Prevotellaceae     11.1 
    ## 7 Streptococcaceae   82.8 
    ## 8 Veillonellaceae     8.59

``` r
relative_abundace %>%
  mutate(Family = factor(Family, levels = sort.class)) %>%
  ggplot(aes(x = Treatment, 
             y = relative_abundance, 
             fill = Family)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~Treatment) +
  theme_minimal()
```

![](Boxplot_taxonomy_files/figure-gfm/load%20your%20data-1.png)<!-- -->

``` r
col_list <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')

relative_abundace %>%
  ggplot(aes(x = Treatment, y = relative_abundance, fill = Family)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = col_list)
```

![](Boxplot_taxonomy_files/figure-gfm/load%20your%20data-2.png)<!-- -->
