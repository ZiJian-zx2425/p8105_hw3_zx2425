Simple document
================

\##Problem2 \#introduce the dataset

``` r
library("p8105.datasets")
acc=read_csv("C:/Users/10145/Desktop/DS project/P8105_hw3_zx2425/p8105_hw3_zx2425/accel_data.csv")
```

    ## Rows: 35 Columns: 1443
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (1): day
    ## dbl (1442): week, day_id, activity.1, activity.2, activity.3, activity.4, ac...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
acc = acc %>% 
  janitor::clean_names()
```

The demensions of the raw dataset is `1443` \* `35`. It contains the
each minute of activity by days and its costs orginized by wide format.

\#clean the dataset

``` r
acc = acc %>%   
  mutate(weedd = case_when(
    day == "Monday" | day=="Tuesday" | day=="Wednesday" | day == "Thursday"| day == "Friday" ~ "weekday",
    day == "Saturday"|day=="Sunday" ~ "weekend" ,
    TRUE     ~ "" ))
acc = acc %>% 
      mutate(
    sum=rowSums(.[4:1443])
  )
acc=acc %>% 
  pivot_longer(activity_1:activity_1440,names_to="act",values_to="cc") 
acc=acc %>% 
separate(act, into = c("activity_name", "minute")) %>%
  mutate(
    minute=as.numeric(minute)
  ) %>% 
select(-activity_name)
acc
```

    ## # A tibble: 50,400 × 7
    ##     week day_id day    weedd       sum minute    cc
    ##    <dbl>  <dbl> <chr>  <chr>     <dbl>  <dbl> <dbl>
    ##  1     1      1 Friday weekday 480543.      1  88.4
    ##  2     1      1 Friday weekday 480543.      2  82.2
    ##  3     1      1 Friday weekday 480543.      3  64.4
    ##  4     1      1 Friday weekday 480543.      4  70.0
    ##  5     1      1 Friday weekday 480543.      5  75.0
    ##  6     1      1 Friday weekday 480543.      6  66.3
    ##  7     1      1 Friday weekday 480543.      7  53.8
    ##  8     1      1 Friday weekday 480543.      8  47.8
    ##  9     1      1 Friday weekday 480543.      9  55.5
    ## 10     1      1 Friday weekday 480543.     10  43.0
    ## # … with 50,390 more rows

The demensions of the raw dataset is 7 \* 50400. It has been translated
to long format dataframe which means the activity variables are
represented by the time that the action is issued and its costs. Also,
there is a weedd variable represent the type of one day (weekend or
not). Furthermore, we sum of one day’s activity that showed in the sum
variable.

``` r
acc_sum <- unique(data.frame('day' = acc$day_id, 
                      'counts'=acc$sum))
ggplot(acc_sum, aes(x = day, y = counts)) + 
  geom_point()  
```

![](DS_homework3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> This
part calculate the total counts for each day, and draw a plot to find if
there are any trends for each day’s activity. The results show that
there are not apparent trend. However, during the 10\~30 days, there are
more regular activities done than at the begining and in the end of
those days.

\#arrange the order and make a plot

``` r
acc_ar=acc %>% 
  arrange(day_id)
acc_ar
```

    ## # A tibble: 50,400 × 7
    ##     week day_id day    weedd       sum minute    cc
    ##    <dbl>  <dbl> <chr>  <chr>     <dbl>  <dbl> <dbl>
    ##  1     1      1 Friday weekday 480543.      1  88.4
    ##  2     1      1 Friday weekday 480543.      2  82.2
    ##  3     1      1 Friday weekday 480543.      3  64.4
    ##  4     1      1 Friday weekday 480543.      4  70.0
    ##  5     1      1 Friday weekday 480543.      5  75.0
    ##  6     1      1 Friday weekday 480543.      6  66.3
    ##  7     1      1 Friday weekday 480543.      7  53.8
    ##  8     1      1 Friday weekday 480543.      8  47.8
    ##  9     1      1 Friday weekday 480543.      9  55.5
    ## 10     1      1 Friday weekday 480543.     10  43.0
    ## # … with 50,390 more rows

``` r
ggplot(acc, aes(x = minute, y = cc , color=day)) + 
  geom_point()  
```

![](DS_homework3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> It we
focus on the distribution of daily points and ignore the difference
among weekdays, it shows that athletes are more active during at time of
a day and the ending of a day. If compared different days during the
week, we can see most of the outline of the figure is composed by green
and red points, which possibly means that people are more active during
weekend. We can conclude that for the patients, they keep an eye on
their daily sport. And individuals always do more excercise at the
morning, noon and evening of a day. They are more likely to get high
level of activity during weekend.

\##problem3 \#introduce data

``` r
data("ny_noaa")
nynoaadat=ny_noaa
```

Firstly, the dataframe contains 2595176 objects and 7 variables.
Variables include id, date, prcp, snow, snwd, tmax, tmin

So the next step lets orginize the dataset and have a outlook.

``` r
nynoaadat = separate(nynoaadat,date, into= c("year","month",'day'),sep= "-")
 
nynoaadat=mutate(
  nynoaadat, year=as.double(year),month=as.double(month),day=as.double(day))
```

The resulting dataset is 2595176 \* 9. The new variables are ‘year’,
‘month’ and ‘day’ which is extract by the data variable. The day_id mark
a unique day. And the ‘prep’,‘snow’, ‘snwd’, are represent the weather
condition which have different measurement.

So next, Let’s unified the measurement standard of ‘prep’, ‘snow’,
‘snwd’ and see the frequency of the ‘snow’.

``` r
nynoaadat=nynoaadat %>% 
  mutate(
    snow=snow*10,
    snwd=snwd*10
  )
nyno_fre=as.data.frame(table(nynoaadat$snow))
nyno_fre
```

    ##       Var1    Freq
    ## 1     -130       1
    ## 2        0 2008508
    ## 3       30    8790
    ## 4       50    9748
    ## 5       80    9962
    ## 6      100    5106
    ## 7      130   23095
    ## 8      150    3672
    ## 9      180    3226
    ## 10     200    4797
    ## 11     230    1959
    ## 12     250   31022
    ## 13     280    2118
    ## 14     300    2814
    ## 15     330    2380
    ## 16     360    1630
    ## 17     380    9197
    ## 18     410    1467
    ## 19     430    1337
    ## 20     460    2123
    ## 21     480     918
    ## 22     510   18274
    ## 23     530    1155
    ## 24     560    1179
    ## 25     580    1198
    ## 26     610     849
    ## 27     640    4506
    ## 28     660     790
    ## 29     690     726
    ## 30     710    1075
    ## 31     740     463
    ## 32     760   10173
    ## 33     790     635
    ## 34     810     811
    ## 35     840     553
    ## 36     860     476
    ## 37     890    2535
    ## 38     910     428
    ## 39     940     404
    ## 40     970     704
    ## 41     990     276
    ## 42    1020    6552
    ## 43    1040     349
    ## 44    1070     504
    ## 45    1090     393
    ## 46    1120     243
    ## 47    1140    1578
    ## 48    1170     276
    ## 49    1190     248
    ## 50    1220     411
    ## 51    1240     183
    ## 52    1270    3901
    ## 53    1300     217
    ## 54    1320     310
    ## 55    1350     253
    ## 56    1370     173
    ## 57    1400     994
    ## 58    1420     187
    ## 59    1450     172
    ## 60    1470     268
    ## 61    1500     124
    ## 62    1520    3131
    ## 63    1550     186
    ## 64    1570     209
    ## 65    1600     149
    ## 66    1630     133
    ## 67    1650     614
    ## 68    1680     115
    ## 69    1700     104
    ## 70    1730     187
    ## 71    1750      80
    ## 72    1780    1650
    ## 73    1800      93
    ## 74    1830     132
    ## 75    1850     117
    ## 76    1880      77
    ## 77    1910     426
    ## 78    1930      70
    ## 79    1960      75
    ## 80    1980     130
    ## 81    2010      60
    ## 82    2030    1475
    ## 83    2060      74
    ## 84    2080      98
    ## 85    2110      69
    ## 86    2130      58
    ## 87    2160     292
    ## 88    2180      55
    ## 89    2210      53
    ## 90    2240      61
    ## 91    2260      35
    ## 92    2290     744
    ## 93    2310      43
    ## 94    2340      52
    ## 95    2360      49
    ## 96    2390      39
    ## 97    2410     192
    ## 98    2440      36
    ## 99    2460      37
    ## 100   2490      58
    ## 101   2510      21
    ## 102   2540     786
    ## 103   2570      34
    ## 104   2590      48
    ## 105   2620      28
    ## 106   2640      24
    ## 107   2670     130
    ## 108   2690      19
    ## 109   2720      22
    ## 110   2740      45
    ## 111   2770      20
    ## 112   2790     369
    ## 113   2820      28
    ## 114   2840      37
    ## 115   2870      22
    ## 116   2900      24
    ## 117   2920      81
    ## 118   2950      20
    ## 119   2970      14
    ## 120   3000      24
    ## 121   3020      22
    ## 122   3050     451
    ## 123   3070      17
    ## 124   3100      29
    ## 125   3120      22
    ## 126   3150      13
    ## 127   3180      70
    ## 128   3200       7
    ## 129   3230      22
    ## 130   3250      12
    ## 131   3280       6
    ## 132   3300     226
    ## 133   3330       9
    ## 134   3350      13
    ## 135   3380      17
    ## 136   3400      13
    ## 137   3430      63
    ## 138   3450      17
    ## 139   3480       6
    ## 140   3510      15
    ## 141   3530      12
    ## 142   3560     235
    ## 143   3580      12
    ## 144   3610      15
    ## 145   3630      14
    ## 146   3660      15
    ## 147   3680      32
    ## 148   3710       4
    ## 149   3730       6
    ## 150   3760      12
    ## 151   3780       5
    ## 152   3810     139
    ## 153   3840       6
    ## 154   3860       8
    ## 155   3890       5
    ## 156   3910       1
    ## 157   3940      27
    ## 158   3960       5
    ## 159   3990       4
    ## 160   4010      10
    ## 161   4040       7
    ## 162   4060     116
    ## 163   4090       6
    ## 164   4110       8
    ## 165   4140      12
    ## 166   4170       9
    ## 167   4190      15
    ## 168   4220       5
    ## 169   4240       3
    ## 170   4270       8
    ## 171   4290       1
    ## 172   4320      63
    ## 173   4340       7
    ## 174   4370       8
    ## 175   4390       3
    ## 176   4450       8
    ## 177   4470       5
    ## 178   4500       5
    ## 179   4520       5
    ## 180   4550       4
    ## 181   4570     100
    ## 182   4600       5
    ## 183   4620       3
    ## 184   4650       5
    ## 185   4670       6
    ## 186   4700      20
    ## 187   4720       4
    ## 188   4750       5
    ## 189   4780       4
    ## 190   4800       2
    ## 191   4830      44
    ## 192   4880       4
    ## 193   4900       2
    ## 194   4950       3
    ## 195   4980       2
    ## 196   5030       2
    ## 197   5050       2
    ## 198   5080      54
    ## 199   5110       2
    ## 200   5130       3
    ## 201   5160       2
    ## 202   5180       3
    ## 203   5210       8
    ## 204   5230       2
    ## 205   5260       2
    ## 206   5280       2
    ## 207   5330      16
    ## 208   5360       1
    ## 209   5440       1
    ## 210   5460       6
    ## 211   5490       4
    ## 212   5510       2
    ## 213   5540       4
    ## 214   5560       1
    ## 215   5590      35
    ## 216   5610       2
    ## 217   5640       2
    ## 218   5660       1
    ## 219   5690       1
    ## 220   5720       3
    ## 221   5740       1
    ## 222   5770       1
    ## 223   5790       1
    ## 224   5840      20
    ## 225   5870       1
    ## 226   5890       1
    ## 227   5920       2
    ## 228   5940       3
    ## 229   5970       4
    ## 230   6070       1
    ## 231   6100      35
    ## 232   6120       1
    ## 233   6150       1
    ## 234   6200       1
    ## 235   6220       2
    ## 236   6250       1
    ## 237   6300       2
    ## 238   6320       3
    ## 239   6350      10
    ## 240   6430       2
    ## 241   6450       1
    ## 242   6480       1
    ## 243   6500       1
    ## 244   6600      13
    ## 245   6630       2
    ## 246   6650       1
    ## 247   6860       6
    ## 248   6930       1
    ## 249   6990       4
    ## 250   7040       1
    ## 251   7110      10
    ## 252   7210       2
    ## 253   7340       1
    ## 254   7370       9
    ## 255   7540       1
    ## 256   7620      17
    ## 257   7750       3
    ## 258   7870       4
    ## 259   8080       1
    ## 260   8100       1
    ## 261   8130       2
    ## 262   8380       2
    ## 263   8430       1
    ## 264   8610       1
    ## 265   8640       2
    ## 266   8710       1
    ## 267   8920       1
    ## 268   9140       4
    ## 269   9400       1
    ## 270   9530       1
    ## 271   9650       1
    ## 272   9780       1
    ## 273  10410       1
    ## 274  10670       1
    ## 275  11050       1
    ## 276  11430       1
    ## 277  12070       1
    ## 278  63500       1
    ## 279  71220       1
    ## 280  77650       1
    ## 281 101600       1

First, we multiply 10 to each variable of ‘snow’ and ‘snwd’, this is
because we do not want to lose data precision compared with the solution
that is to divide by 10. At the same time, after viewing the frequency
table of snow we can see the value of 0 is significantly larger than
other values, This possibly because the possible of the snow of the city
is low. Besides, we are comparing the frequency of no snow with the
frequency of snow at a certain level at the city.

``` r
nynoaadat %>% 
  filter(month==1 | month== 7) %>% 
  drop_na(tmax) %>% 
  group_by(month,id,year) %>% 
  summarize(
      average_tmax=mean(tmax)
    ) %>% 
  ggplot(aes(x=year,y=average_tmax,color=month))+geom_point()+
  facet_grid(.~month)
```

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## Warning in mean.default(tmax): 参数不是数值也不是逻辑值：回覆NA

    ## `summarise()` has grouped output by 'month', 'id'. You can override using the
    ## `.groups` argument.

    ## Warning: Removed 8141 rows containing missing values (geom_point).

![](DS_homework3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
According to the plot, we can see the range of tmax in January is
\[-100,100\], and July is \[200,310\]. The range is pretty different
either. On January is about 200, and on July is about 110 which
represent the temperature difference in January is higher than in July.
Both of them has outliers for exaple there are two extremely cold day on
January which should pay more attention to the reason: wheather is the
wrong data or not.
