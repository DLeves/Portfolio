Budapesti lakásárak elemzése
================
Dittrich Levente
2023-06-10

- <a href="#kezdeti-beállítások" id="toc-kezdeti-beállítások">Kezdeti
  beállítások</a>
  - <a href="#használt-package-ek" id="toc-használt-package-ek">Használt
    package-ek</a>
  - <a href="#adatok-megszerzése" id="toc-adatok-megszerzése">Adatok
    megszerzése</a>
  - <a href="#adatok-átalakítása" id="toc-adatok-átalakítása">Adatok
    átalakítása</a>
- <a href="#adatvizualizáció"
  id="toc-adatvizualizáció">Adatvizualizáció</a>
- <a href="#kointegréció-tesztelése"
  id="toc-kointegréció-tesztelése">Kointegréció tesztelése</a>
- <a href="#optimális-késleltetés-megtalálása"
  id="toc-optimális-késleltetés-megtalálása">Optimális késleltetés
  megtalálása</a>
  - <a href="#stacionaritás" id="toc-stacionaritás">Stacionaritás</a>
  - <a href="#késleltetés-meghatározása"
    id="toc-késleltetés-meghatározása">Késleltetés meghatározása</a>
- <a href="#modellépítés" id="toc-modellépítés">Modellépítés</a>

Ebben a portfolió fejezetben azt vizsgálom meg, hogy az
[AMD](https://finance.yahoo.com/quote/AMD?p=AMD&.tsrc=fin-srch),
[NVIDIA](https://finance.yahoo.com/quote/NVDA?p=NVDA&.tsrc=fin-srch) és
[Intel](https://finance.yahoo.com/quote/INTC?p=INTC&.tsrc=fin-srch)
vállalatok részvényeinak árfolyamára fogok VECM vagy VAR modellt
illeszteni, attól függően, hogy van-e közös hosszútávú pályájuk.

Mindhárom vállalat székhelye a kaliforniai Santa Claraban található,
azonban nem csak az a közös bennük, hanem a tevékenységük is hasonló.
Ezek a vállalatok mind mikro- és grafikus proceszorokkal foglalkoznak
elsősorban. A napi gyakoriságú részvényadatokat a Yahoo Finance-ről
töltöm le, viszont egy meghatározott intervallumban, így
reprodukálhatóak lesznek eredményeim.

# Kezdeti beállítások

## Használt package-ek

``` r
library(quantmod)
library(tidyverse)
library(knitr)
library(urca)
library(lmtest)
library(aTSA)
library(vars)
library(tsDyn)
```

Egy részvénynek egy napra több értékei is lehet, például az adott napi
maximum, minimum, a nyitó vagy záró értéke. A továbbiakban az egyes
részvények záró értékeivel fogok dolgozni.

## Adatok megszerzése

``` r
intel = getSymbols("INTC", src = "yahoo", auto.assign = F, from = "2010-01-01", to = "2023-06-01")
amd = getSymbols("AMD", src = "yahoo", auto.assign = F, from = "2010-01-01", to = "2023-06-01")
nvidia = getSymbols("NVDA", src = "yahoo", auto.assign = F, from = "2010-01-01", to = "2023-06-01")
```

## Adatok átalakítása

Érdemes az adatokat nem xts formátumban egyenként, hanem dataframe-ként
egyben tárolni, hogy később könnyebb legyen a munka.

``` r
df = data.frame(
  time = index(intel),
  intc = as.numeric(intel$INTC.Close),
  amd = as.numeric(amd$AMD.Close),
  nvda = as.numeric(nvidia$NVDA.Close)
)
rm(intel, amd, nvidia)
kable(head(df))
```

| time       |  intc |  amd |   nvda |
|:-----------|------:|-----:|-------:|
| 2010-01-04 | 20.88 | 9.70 | 4.6225 |
| 2010-01-05 | 20.87 | 9.71 | 4.6900 |
| 2010-01-06 | 20.80 | 9.57 | 4.7200 |
| 2010-01-07 | 20.60 | 9.47 | 4.6275 |
| 2010-01-08 | 20.83 | 9.43 | 4.6375 |
| 2010-01-11 | 20.95 | 9.14 | 4.5725 |

Az adatokat tartalmazó dataframe változói a következők:

| Változó neve | Leírás                                    | Mértékegység |
|:-------------|:------------------------------------------|:-------------|
| time         | Idő(az adott nap)                         | Dátum        |
| intc         | Az adott napi Intel részvény záró értéke  | \$           |
| amd          | Az adott napi AMD részvény záró értéke    | \$           |
| nvda         | Az adott napi NVIDIA részvény záró értéke | \$           |

3375 megfigyelés van mindhárom változóból, ez mindenképpen elegendő lesz
a modellépítéshez.

# Adatvizualizáció

``` r
ggplot(df, aes(x = time))+
  geom_line(aes(y = intc, col = "Intel"))+
  geom_line(aes(y = amd, col = "AMD"))+
  geom_line(aes(y = nvda, col = "NVIDIA"))+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_color_manual(values = c("#ED1C24", "#0071C5", "#76B900"))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  labs(x = "Idő", y = "Részvény árfolyam")
```

![](vecm_hun_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Már a grafikonon is látszik, hogy valószínűleg nem stacionerek az
idősorok.

# Kointegréció tesztelése

Ahhoz, hogy tudja, hogy VECM vagy VAR modellt kell használjak, meg kell
állapítanom, hogy van-e közös hosszútávú pályájuk az idősoroknak.
Kointegráció esetén VECM modell a megfelelő, míg ha nincs közös távú
hosszútávú pályája az idósoroknak, akkor VAR modellt kell alkalmazzak.

Erre egy *Johansen-tesztet* fogok végezni. A teszt érdekessége, hogy
több nullhipotézist is felvet. Ez esetben a nullhipotézisek a
következők, melyekben *r* a közös hosszútávú pályák száma:

- H0: $r = 0$ \| H1: $r > 0$
- H0: $r \le 1$ \| H1: $r > 1$
- H0: $r \le 2$ \| H1: $r > 2$

Mivel összesen 3 db idősorom van, ezért maximum 2 db közös hosszútávú
pálya lehetséges (a kódban ez a *K* paraméter).

``` r
johansen_test = ca.jo(df[,-1], type = "eigen",  K = 2, ecdet = "const", spec = "longrun")
summary(johansen_test)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: maximal eigenvalue statistic (lambda max) , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 5.063385e-03 2.381383e-03 8.799114e-04 2.288984e-19
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 2 |  2.97  7.52  9.24 12.97
    ## r <= 1 |  8.04 13.75 15.67 20.20
    ## r = 0  | 17.12 19.77 22.00 26.81
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##               intc.l2      amd.l2     nvda.l2  constant
    ## intc.l2    1.00000000   1.0000000   1.0000000  1.000000
    ## amd.l2     0.06608674  -1.0293819   1.2081244  2.332413
    ## nvda.l2   -0.40668645   0.4136885  -0.4548912 -1.247798
    ## constant -26.42227831 -27.1668824 -57.8211496  1.023926
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##              intc.l2        amd.l2       nvda.l2      constant
    ## intc.d  0.0004994377 -0.0007538154 -0.0008416566 -1.526424e-19
    ## amd.d  -0.0004713206  0.0046726538 -0.0010653154  1.737848e-18
    ## nvda.d -0.0060660953  0.0048779829 -0.0023863493  7.045692e-18

A teszt egy jobboldali próba, ez azt jelenti, hogy akkor tudjuk
elfogadni a nullhipotéziseket, ha az adott próbafüggvény kisebb vagy
egyenlő a kritikus értéknél. $H0: r = 0$ Minden szokványos
szignifikanciaszinten elfogadható. $H0: r \le 1$ Minden szokványos
szignifikanciaszinten elfogadható. $H0: r \le 2$ Minden szokványos
szignifikanciaszinten elfogadható.

Ilyen esetben én a kettő közös hosszútávú pályát venném kiindulópontnak,
amennyiben nem lesz szignifikáns ECT2, akkor megfontolom az egy
hosszútávú pályára váltást.

# Optimális késleltetés megtalálása

Meg kellene határozzam az optimális késleltetést a VECM modellhez. Ez
több lépésben fog megtörténni, először az idősorok stacionaritását kell
tesztelnem. Amennyiben van egységgyök az idősorban, akkor stacionerré
kell alakítanom őket majd mintha VAR modellhez keresném, meg kell
határozzam az optimális késleltetést az információs kritériumokat
felhasználva.

## Stacionaritás

Az erős stacionaritás azt jelenti, hogy az idősorok minden véges
dimenziós eloszlása eltolásinvariás. Ez egy olyan erős követelmény, amit
inkább a valószínűségszámításban használnak gyakran, azonban
ökonometriában túl szigorú követelmény, ezért szokás gyenge
stacionaritással dolgozni. Ez azt jelenti, hogy az idősor szórása és
várható értéke időben állandó. A stacionaritás teszteléséhez egy
Augmented Dickey-Fuller tesztet fogok végezni.

Az ADF teszt hipotézisei:

- H0: Az idősor nem stacioner, $\phi = 0$
- H1: Az idősor stacionárius, $\phi \neq 0$

``` r
adf.test(df$intc)
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag    ADF p.value
    ##  [1,]   0 -0.459   0.512
    ##  [2,]   1 -0.346   0.544
    ##  [3,]   2 -0.379   0.535
    ##  [4,]   3 -0.405   0.527
    ##  [5,]   4 -0.386   0.533
    ##  [6,]   5 -0.388   0.532
    ##  [7,]   6 -0.345   0.545
    ##  [8,]   7 -0.383   0.534
    ##  [9,]   8 -0.336   0.547
    ## Type 2: with drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -2.02   0.320
    ##  [2,]   1 -1.78   0.417
    ##  [3,]   2 -1.85   0.387
    ##  [4,]   3 -1.93   0.358
    ##  [5,]   4 -1.87   0.379
    ##  [6,]   5 -1.87   0.381
    ##  [7,]   6 -1.80   0.409
    ##  [8,]   7 -1.86   0.383
    ##  [9,]   8 -1.73   0.434
    ## Type 3: with drift and trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -2.23   0.479
    ##  [2,]   1 -1.70   0.704
    ##  [3,]   2 -1.87   0.632
    ##  [4,]   3 -2.02   0.566
    ##  [5,]   4 -1.92   0.610
    ##  [6,]   5 -1.92   0.611
    ##  [7,]   6 -1.74   0.689
    ##  [8,]   7 -1.91   0.617
    ##  [9,]   8 -1.65   0.727
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

``` r
adf.test(df$amd)
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 0.647   0.830
    ##  [2,]   1 0.720   0.851
    ##  [3,]   2 0.787   0.870
    ##  [4,]   3 0.813   0.878
    ##  [5,]   4 0.779   0.868
    ##  [6,]   5 0.774   0.867
    ##  [7,]   6 0.747   0.859
    ##  [8,]   7 0.610   0.819
    ##  [9,]   8 0.642   0.829
    ## Type 2: with drift no trend 
    ##       lag      ADF p.value
    ##  [1,]   0 -0.11653   0.944
    ##  [2,]   1 -0.04887   0.952
    ##  [3,]   2  0.01145   0.957
    ##  [4,]   3  0.03446   0.958
    ##  [5,]   4  0.00386   0.956
    ##  [6,]   5 -0.00259   0.955
    ##  [7,]   6 -0.03059   0.953
    ##  [8,]   7 -0.15106   0.939
    ##  [9,]   8 -0.12257   0.943
    ## Type 3: with drift and trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -1.92   0.612
    ##  [2,]   1 -1.86   0.638
    ##  [3,]   2 -1.80   0.662
    ##  [4,]   3 -1.78   0.671
    ##  [5,]   4 -1.81   0.660
    ##  [6,]   5 -1.81   0.660
    ##  [7,]   6 -1.82   0.653
    ##  [8,]   7 -1.95   0.599
    ##  [9,]   8 -1.92   0.612
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

``` r
adf.test(df$nvda)
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag  ADF p.value
    ##  [1,]   0 2.76    0.99
    ##  [2,]   1 2.83    0.99
    ##  [3,]   2 3.04    0.99
    ##  [4,]   3 3.25    0.99
    ##  [5,]   4 3.41    0.99
    ##  [6,]   5 3.07    0.99
    ##  [7,]   6 2.75    0.99
    ##  [8,]   7 2.54    0.99
    ##  [9,]   8 2.59    0.99
    ## Type 2: with drift no trend 
    ##       lag  ADF p.value
    ##  [1,]   0 2.04    0.99
    ##  [2,]   1 2.11    0.99
    ##  [3,]   2 2.31    0.99
    ##  [4,]   3 2.51    0.99
    ##  [5,]   4 2.66    0.99
    ##  [6,]   5 2.35    0.99
    ##  [7,]   6 2.06    0.99
    ##  [8,]   7 1.86    0.99
    ##  [9,]   8 1.92    0.99
    ## Type 3: with drift and trend 
    ##       lag    ADF p.value
    ##  [1,]   0 0.2083    0.99
    ##  [2,]   1 0.2819    0.99
    ##  [3,]   2 0.4803    0.99
    ##  [4,]   3 0.6804    0.99
    ##  [5,]   4 0.8161    0.99
    ##  [6,]   5 0.5466    0.99
    ##  [7,]   6 0.2765    0.99
    ##  [8,]   7 0.0869    0.99
    ##  [9,]   8 0.1436    0.99
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

``` r
adf.test(diff(df$intc))
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -68.1    0.01
    ##  [2,]   1 -42.0    0.01
    ##  [3,]   2 -32.5    0.01
    ##  [4,]   3 -29.2    0.01
    ##  [5,]   4 -26.1    0.01
    ##  [6,]   5 -25.2    0.01
    ##  [7,]   6 -21.9    0.01
    ##  [8,]   7 -22.2    0.01
    ##  [9,]   8 -19.1    0.01
    ## Type 2: with drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -68.0    0.01
    ##  [2,]   1 -42.0    0.01
    ##  [3,]   2 -32.5    0.01
    ##  [4,]   3 -29.2    0.01
    ##  [5,]   4 -26.1    0.01
    ##  [6,]   5 -25.2    0.01
    ##  [7,]   6 -21.9    0.01
    ##  [8,]   7 -22.2    0.01
    ##  [9,]   8 -19.1    0.01
    ## Type 3: with drift and trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -68.1    0.01
    ##  [2,]   1 -42.0    0.01
    ##  [3,]   2 -32.5    0.01
    ##  [4,]   3 -29.2    0.01
    ##  [5,]   4 -26.1    0.01
    ##  [6,]   5 -25.2    0.01
    ##  [7,]   6 -22.0    0.01
    ##  [8,]   7 -22.2    0.01
    ##  [9,]   8 -19.1    0.01
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

``` r
adf.test(diff(df$amd))
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -59.4    0.01
    ##  [2,]   1 -42.4    0.01
    ##  [3,]   2 -34.5    0.01
    ##  [4,]   3 -29.2    0.01
    ##  [5,]   4 -26.0    0.01
    ##  [6,]   5 -23.4    0.01
    ##  [7,]   6 -20.4    0.01
    ##  [8,]   7 -19.5    0.01
    ##  [9,]   8 -18.1    0.01
    ## Type 2: with drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -59.4    0.01
    ##  [2,]   1 -42.4    0.01
    ##  [3,]   2 -34.5    0.01
    ##  [4,]   3 -29.2    0.01
    ##  [5,]   4 -26.0    0.01
    ##  [6,]   5 -23.4    0.01
    ##  [7,]   6 -20.4    0.01
    ##  [8,]   7 -19.5    0.01
    ##  [9,]   8 -18.1    0.01
    ## Type 3: with drift and trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -59.4    0.01
    ##  [2,]   1 -42.4    0.01
    ##  [3,]   2 -34.6    0.01
    ##  [4,]   3 -29.3    0.01
    ##  [5,]   4 -26.0    0.01
    ##  [6,]   5 -23.4    0.01
    ##  [7,]   6 -20.4    0.01
    ##  [8,]   7 -19.5    0.01
    ##  [9,]   8 -18.2    0.01
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

``` r
adf.test(diff(df$nvda))
```

    ## Augmented Dickey-Fuller Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -58.3    0.01
    ##  [2,]   1 -42.7    0.01
    ##  [3,]   2 -35.7    0.01
    ##  [4,]   3 -31.0    0.01
    ##  [5,]   4 -24.5    0.01
    ##  [6,]   5 -20.1    0.01
    ##  [7,]   6 -17.5    0.01
    ##  [8,]   7 -16.9    0.01
    ##  [9,]   8 -15.4    0.01
    ## Type 2: with drift no trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -58.4    0.01
    ##  [2,]   1 -42.7    0.01
    ##  [3,]   2 -35.8    0.01
    ##  [4,]   3 -31.1    0.01
    ##  [5,]   4 -24.6    0.01
    ##  [6,]   5 -20.2    0.01
    ##  [7,]   6 -17.6    0.01
    ##  [8,]   7 -17.0    0.01
    ##  [9,]   8 -15.5    0.01
    ## Type 3: with drift and trend 
    ##       lag   ADF p.value
    ##  [1,]   0 -58.5    0.01
    ##  [2,]   1 -42.8    0.01
    ##  [3,]   2 -35.9    0.01
    ##  [4,]   3 -31.3    0.01
    ##  [5,]   4 -24.7    0.01
    ##  [6,]   5 -20.4    0.01
    ##  [7,]   6 -17.8    0.01
    ##  [8,]   7 -17.1    0.01
    ##  [9,]   8 -15.7    0.01
    ## ---- 
    ## Note: in fact, p.value = 0.01 means p.value <= 0.01

## Késleltetés meghatározása

``` r
df$d_intc = c(NA, diff(df$intc))
df$d_amd = c(NA, diff(df$amd))
df$d_nvda = c(NA, diff(df$nvda))

VARselect(df[-1,5:7], lag.max = 20)
```

    ## $selection
    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##     19     11      1     19 
    ## 
    ## $criteria
    ##               1        2        3        4        5        6        7        8
    ## AIC(n) 1.934359 1.929300 1.920119 1.918221 1.905830 1.890258 1.886351 1.871645
    ## HQ(n)  1.942187 1.943000 1.939690 1.943665 1.937145 1.927444 1.929409 1.920575
    ## SC(n)  1.956247 1.967605 1.974840 1.989360 1.993385 1.994229 2.006739 2.008449
    ## FPE(n) 6.919605 6.884689 6.821767 6.808836 6.724987 6.621077 6.595263 6.498981
    ##               9       10       11       12       13       14       15       16
    ## AIC(n) 1.865179 1.853727 1.840811 1.842996 1.841276 1.842258 1.839574 1.838557
    ## HQ(n)  1.919980 1.914401 1.907355 1.915412 1.919564 1.926417 1.929605 1.934460
    ## SC(n)  2.018400 2.023365 2.026865 2.045467 2.060164 2.077562 2.091295 2.106694
    ## FPE(n) 6.457098 6.383580 6.301659 6.315448 6.304600 6.310797 6.293891 6.287498
    ##              17       18       19       20
    ## AIC(n) 1.827059 1.821188 1.817967 1.818541
    ## HQ(n)  1.928834 1.928833 1.931484 1.937930
    ## SC(n)  2.111613 2.122158 2.135354 2.152345
    ## FPE(n) 6.215629 6.179247 6.159387 6.162934

# Modellépítés

``` r
alapmodell = VECM(df[,2:4], lag = 11, r = 2)
```

    ## Warning in lineVar(data, lag, r = r, include = include, model = "VECM", : Estimation of more than 1 coint relationship is not possible with estim '2OLS'. Switched to Johansen 'ML'

``` r
summary(alapmodell)
```

    ## #############
    ## ###Model VECM 
    ## #############
    ## Full sample size: 3375   End sample size: 3363
    ## Number of variables: 3   Number of estimated slope parameters 108
    ## AIC 6161.923     BIC 6835.188    SSR 46727.5
    ## Cointegrating vector (estimated by ML):
    ##             intc          amd       nvda
    ## r1  1.000000e+00 1.110223e-16 -0.3736951
    ## r2 -1.176903e-16 1.000000e+00 -0.7300483
    ## 
    ## 
    ##               ECT1                ECT2                Intercept         
    ## Equation intc -0.0003(0.0011)     0.0011(0.0012)      0.0207(0.0336)    
    ## Equation amd  0.0051(0.0022)*     -0.0063(0.0024)**   -0.1225(0.0664).  
    ## Equation nvda 0.0008(0.0048)      -0.0060(0.0051)     0.0109(0.1420)    
    ##               intc -1            amd -1              nvda -1            
    ## Equation intc -0.1306(0.0186)*** -0.0548(0.0136)***  0.0231(0.0064)***  
    ## Equation amd  -0.1616(0.0367)*** -0.0173(0.0268)     0.0145(0.0125)     
    ## Equation nvda -0.3299(0.0786)*** 0.0180(0.0573)      -0.0056(0.0268)    
    ##               intc -2            amd -2              nvda -2            
    ## Equation intc 0.0448(0.0188)*    0.0127(0.0136)      -0.0038(0.0064)    
    ## Equation amd  0.0531(0.0370)     0.0289(0.0269)      -0.0437(0.0126)*** 
    ## Equation nvda 0.0506(0.0792)     -0.0348(0.0575)     -0.0381(0.0269)    
    ##               intc -3             amd -3             nvda -3            
    ## Equation intc 0.0628(0.0187)***   -0.0568(0.0136)*** 0.0182(0.0064)**   
    ## Equation amd  -0.0388(0.0369)     0.0259(0.0269)     -0.0167(0.0126)    
    ## Equation nvda 0.0591(0.0789)      0.1425(0.0576)*    -0.0852(0.0270)**  
    ##               intc -4             amd -4              nvda -4            
    ## Equation intc -0.0224(0.0189)     -0.0102(0.0140)     -0.0010(0.0070)    
    ## Equation amd  -0.0307(0.0373)     0.0194(0.0276)      -0.0003(0.0139)    
    ## Equation nvda 0.0256(0.0798)      -0.0380(0.0592)     -0.0309(0.0297)    
    ##               intc -5             amd -5              nvda -5           
    ## Equation intc -0.0134(0.0189)     -0.0012(0.0140)     0.0068(0.0070)    
    ## Equation amd  -0.0464(0.0373)     -0.0083(0.0277)     0.0097(0.0139)    
    ## Equation nvda -0.0631(0.0798)     -0.0551(0.0592)     0.1148(0.0298)*** 
    ##               intc -6            amd -6              nvda -6            
    ## Equation intc -0.0627(0.0189)*** 0.0179(0.0140)      -0.0021(0.0071)    
    ## Equation amd  -0.0974(0.0372)**  -0.0889(0.0276)**   0.0746(0.0139)***  
    ## Equation nvda -0.2353(0.0796)**  -0.0825(0.0591)     0.1425(0.0298)***  
    ##               intc -7             amd -7             nvda -7           
    ## Equation intc 0.0143(0.0189)      0.0267(0.0140).    0.0017(0.0071)    
    ## Equation amd  -0.0244(0.0373)     0.0444(0.0276)     0.0110(0.0140)    
    ## Equation nvda -0.0986(0.0798)     0.1487(0.0590)*    0.0115(0.0299)    
    ##               intc -8            amd -8             nvda -8           
    ## Equation intc -0.0538(0.0189)**  0.0028(0.0140)     -0.0134(0.0071).  
    ## Equation amd  -0.1141(0.0373)**  0.0669(0.0276)*    -0.0349(0.0139)*  
    ## Equation nvda -0.2454(0.0798)**  0.2980(0.0590)***  -0.1075(0.0298)***
    ##               intc -9             amd -9              nvda -9            
    ## Equation intc 0.0963(0.0189)***   -0.0057(0.0140)     -0.0034(0.0071)    
    ## Equation amd  -0.0255(0.0372)     -0.0243(0.0277)     0.0211(0.0140)     
    ## Equation nvda -0.1041(0.0796)     0.0253(0.0592)      0.0301(0.0299)     
    ##               intc -10            amd -10             nvda -10           
    ## Equation intc -0.0051(0.0189)     0.0225(0.0140)      -0.0093(0.0071)    
    ## Equation amd  0.0421(0.0374)      -0.0027(0.0277)     -0.0429(0.0140)**  
    ## Equation nvda 0.1181(0.0800)      -0.2010(0.0592)***  0.0159(0.0299)     
    ##               intc -11            amd -11             nvda -11           
    ## Equation intc -0.0112(0.0188)     -0.0403(0.0141)**   0.0184(0.0071)**   
    ## Equation amd  -0.0388(0.0370)     0.0318(0.0278)      -0.0036(0.0140)    
    ## Equation nvda -0.0821(0.0792)     -0.0406(0.0595)     0.1108(0.0300)***

``` r
szukitett_modell = VECM(df[,2:4], lag = 11, r = 1)
summary(szukitett_modell)
```

    ## #############
    ## ###Model VECM 
    ## #############
    ## Full sample size: 3375   End sample size: 3363
    ## Number of variables: 3   Number of estimated slope parameters 105
    ## AIC 6167.099     BIC 6822.002    SSR 46780.28
    ## Cointegrating vector (estimated by 2OLS):
    ##    intc        amd     nvda
    ## r1    1 -0.9303583 0.161078
    ## 
    ## 
    ##               ECT                 Intercept           intc -1           
    ## Equation intc 0.0006(0.0008)      -0.0093(0.0198)     -0.1307(0.0186)***
    ## Equation amd  0.0022(0.0015)      -0.0086(0.0390)     -0.1624(0.0368)***
    ## Equation nvda -0.0022(0.0032)     0.1341(0.0835)      -0.3326(0.0786)***
    ##               amd -1              nvda -1            intc -2           
    ## Equation intc -0.0537(0.0136)***  0.0222(0.0063)***  0.0449(0.0188)*   
    ## Equation amd  -0.0202(0.0268)     0.0173(0.0125)     0.0520(0.0370)    
    ## Equation nvda 0.0117(0.0573)      0.0001(0.0267)     0.0475(0.0792)    
    ##               amd -2              nvda -2             intc -3            
    ## Equation intc 0.0139(0.0136)      -0.0047(0.0063)     0.0628(0.0187)***  
    ## Equation amd  0.0258(0.0268)      -0.0408(0.0125)**   -0.0397(0.0369)    
    ## Equation nvda -0.0416(0.0574)     -0.0323(0.0268)     0.0559(0.0789)     
    ##               amd -3             nvda -3             intc -4            
    ## Equation intc -0.0556(0.0136)*** 0.0173(0.0064)**    -0.0227(0.0189)    
    ## Equation amd  0.0227(0.0269)     -0.0137(0.0125)     -0.0310(0.0373)    
    ## Equation nvda 0.1357(0.0575)*    -0.0793(0.0268)**   0.0238(0.0798)     
    ##               amd -4              nvda -4             intc -5            
    ## Equation intc -0.0092(0.0140)     -0.0017(0.0070)     -0.0136(0.0189)    
    ## Equation amd  0.0167(0.0276)      0.0020(0.0139)      -0.0467(0.0373)    
    ## Equation nvda -0.0437(0.0591)     -0.0264(0.0297)     -0.0649(0.0798)    
    ##               amd -5              nvda -5            intc -6           
    ## Equation intc -0.0002(0.0140)     0.0062(0.0070)     -0.0628(0.0189)***
    ## Equation amd  -0.0111(0.0276)     0.0121(0.0139)     -0.0978(0.0372)** 
    ## Equation nvda -0.0608(0.0591)     0.1194(0.0297)***  -0.2374(0.0796)** 
    ##               amd -6              nvda -6             intc -7            
    ## Equation intc 0.0189(0.0140)      -0.0027(0.0070)     0.0141(0.0189)     
    ## Equation amd  -0.0916(0.0276)***  0.0769(0.0139)***   -0.0249(0.0373)    
    ## Equation nvda -0.0880(0.0591)     0.1470(0.0298)***   -0.1006(0.0798)    
    ##               amd -7             nvda -7            intc -8           
    ## Equation intc 0.0277(0.0140)*    0.0010(0.0071)     -0.0540(0.0189)** 
    ## Equation amd  0.0416(0.0276)     0.0134(0.0139)     -0.1146(0.0373)** 
    ## Equation nvda 0.1428(0.0590)*    0.0161(0.0298)     -0.2474(0.0798)** 
    ##               amd -8             nvda -8            intc -9            
    ## Equation intc 0.0038(0.0140)     -0.0141(0.0070)*   0.0960(0.0189)***  
    ## Equation amd  0.0642(0.0276)*    -0.0325(0.0139)*   -0.0257(0.0372)    
    ## Equation nvda 0.2923(0.0590)***  -0.1030(0.0297)*** -0.1056(0.0796)    
    ##               amd -9              nvda -9             intc -10           
    ## Equation intc -0.0047(0.0140)     -0.0041(0.0071)     -0.0055(0.0189)    
    ## Equation amd  -0.0272(0.0276)     0.0234(0.0139).     0.0424(0.0374)     
    ## Equation nvda 0.0193(0.0591)      0.0346(0.0298)      0.1173(0.0800)     
    ##               amd -10             nvda -10            intc -11           
    ## Equation intc 0.0235(0.0140).     -0.0099(0.0071)     -0.0117(0.0188)    
    ## Equation amd  -0.0056(0.0276)     -0.0407(0.0139)**   -0.0385(0.0370)    
    ## Equation nvda -0.2071(0.0592)***  0.0202(0.0298)      -0.0826(0.0792)    
    ##               amd -11             nvda -11           
    ## Equation intc -0.0392(0.0141)**   0.0178(0.0071)*    
    ## Equation amd  0.0289(0.0278)      -0.0014(0.0140)    
    ## Equation nvda -0.0466(0.0595)     0.1151(0.0299)***
