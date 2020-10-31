Stack Barplot for taxonomy
================
Sebastián Sáenz
30/10/2020

We are going to use **ggplot** to create our barplot. Load the library
**tidyverse** or install it in case you have not done it before.

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

Next we are going to load our data. The relative abundance of the
different taxa were previously calculated.

**check.names** prevent that R would change your column names. Use
**str()** to get information about your data frame. In this case we have
17 rows and 8 columns. **str()** also show you what kind of variable you
have.

``` r
df <- read.table("mock_tax.txt",
                 header = TRUE,
                 sep = "\t",
                 check.names = FALSE)

str(df)
```

    ## 'data.frame':    17 obs. of  8 variables:
    ##  $ Phylum    : Factor w/ 4 levels "Actinobacteria",..: 2 1 3 3 3 1 4 3 3 3 ...
    ##  $ Family    : Factor w/ 8 levels "Bacteroidaceae",..: 6 2 3 5 7 2 4 7 7 7 ...
    ##  $ Control   : num  3.18 3.95 6.69 11.78 5.18 ...
    ##  $ Control   : num  10.06 8.33 4.21 3.31 10.53 ...
    ##  $ Control   : num  1.12 5.66 7.11 5.45 3.61 ...
    ##  $ Antibiotic: num  7.1 2.4 4.12 8.9 4.7 ...
    ##  $ Antibiotic: num  3.43 8 4.82 5.91 1.89 ...
    ##  $ Antibiotic: num  8.29 9.64 3.23 8.27 6.26 ...

Let´s check if the data was imported correctly by summing the abundance
of every taxa in each sample. The result must be 100 per column.

``` r
colSums(df[3:8])
```

    ##      Control    Control.1    Control.2   Antibiotic Antibiotic.1 Antibiotic.2 
    ##          100          100          100          100          100          100

Next we transform our df to a longer format as it is easier to use with
**ggplot**. We create two new variables **Treatment** and **Abundance**.
Treatment would receive all the column names and Abundance all the
values.

``` r
df_long <- pivot_longer(df, cols = c(3:4),
             names_to = "Treatment",
             values_to ="Abundance")

str(df_long)
```

    ## tibble [102 x 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Phylum   : Factor w/ 4 levels "Actinobacteria",..: 2 2 2 2 2 2 1 1 1 1 ...
    ##  $ Family   : Factor w/ 8 levels "Bacteroidaceae",..: 6 6 6 6 6 6 2 2 2 2 ...
    ##  $ Treatment: chr [1:102] "Control" "Control" "Control" "Antibiotic" ...
    ##  $ Abundance: num [1:102] 3.18 10.06 1.12 7.1 3.43 ...

Now we have 102 rows and 4 columns.

After, we calculate the mean relative abundance of every Family among
the treatments. We can use **group\_by()** to establish groups of data.
Then those groups would be summarise with a new variable that we create.
In this case the new variable is **relative\_abundance**

``` r
relative_abundace <- df_long %>%
    group_by(Family, Treatment) %>%
    summarise(relative_abundance = sum(Abundance)/3)
```

    ## `summarise()` regrouping output by 'Family' (override with `.groups` argument)

``` r
relative_abundace
```

    ## # A tibble: 16 x 3
    ## # Groups:   Family [8]
    ##    Family             Treatment  relative_abundance
    ##    <fct>              <chr>                   <dbl>
    ##  1 Bacteroidaceae     Antibiotic              12.1 
    ##  2 Bacteroidaceae     Control                  8.15
    ##  3 Bifidobacteriaceae Antibiotic              13.3 
    ##  4 Bifidobacteriaceae Control                 15.0 
    ##  5 Clostridiaceae     Antibiotic               4.05
    ##  6 Clostridiaceae     Control                  6.00
    ##  7 Helicobacteraceae  Antibiotic               4.80
    ##  8 Helicobacteraceae  Control                  4.51
    ##  9 Lactobacillaceae   Antibiotic              16.7 
    ## 10 Lactobacillaceae   Control                 13.0 
    ## 11 Prevotellaceae     Antibiotic               6.27
    ## 12 Prevotellaceae     Control                  4.79
    ## 13 Streptococcaceae   Antibiotic              37.6 
    ## 14 Streptococcaceae   Control                 45.1 
    ## 15 Veillonellaceae    Antibiotic               5.18
    ## 16 Veillonellaceae    Control                  3.42

## Ploting

Finally, we can plot our data. Lets try 3 different versions of barplot.
In all of them we have to use **geom\_bar(stat = ‘identity’)**, because
we do not want any statistical transformation of our data.

### 1\) Normal stack barplot.

``` r
relative_abundace %>%
  ggplot(aes(x = Treatment, y = relative_abundance, fill = Family)) +
  geom_bar(stat = 'identity') +
  ylab("Relative abundance (%)") +
  xlab("") +
  theme_classic()
```

![](Boxplot_taxonomy_files/figure-gfm/Normal%20plot-1.png)<!-- -->

### 2\) Unstack bars using **position = position\_dodge()**.

``` r
relative_abundace %>%
  ggplot(aes(x = Treatment, y = relative_abundance, fill = Family)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  ylab("Relative abundance (%)") +
  xlab("") +
  theme_classic()
```

![](Boxplot_taxonomy_files/figure-gfm/Unstack%20bars-1.png)<!-- -->

### 3\) An independent plot for every Family using **facet\_wrap(\~Family)**.

``` r
relative_abundace %>%
  ggplot(aes(x = Treatment, y = relative_abundance, fill = Family)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  ylab("Relative abundance (%)") +
  xlab("") +
  facet_wrap(~Family) +
  theme_bw()
```

![](Boxplot_taxonomy_files/figure-gfm/facet%20plot-1.png)<!-- -->
