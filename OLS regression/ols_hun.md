Budapesti lakásárak elemzése
================
Dittrich Levente
2023-06-08

- <a href="#kezdeti-beállítások" id="toc-kezdeti-beállítások">Kezdeti
  beállítások</a>
  - <a href="#használt-library-k" id="toc-használt-library-k">Használt
    library-k</a>
  - <a href="#adatok-beolvasása" id="toc-adatok-beolvasása">Adatok
    beolvasása</a>
  - <a href="#adatok-átalakítása" id="toc-adatok-átalakítása">Adatok
    átalakítása</a>
- <a href="#leíró-statisztika-adatvizualizációk"
  id="toc-leíró-statisztika-adatvizualizációk">Leíró statisztika,
  adatvizualizációk</a>
- <a href="#korreláció" id="toc-korreláció">Korreláció</a>
- <a href="#modellépítés" id="toc-modellépítés">Modellépítés</a>
  - <a href="#alapmodell" id="toc-alapmodell">Alapmodell</a>
  - <a href="#modellszelekció" id="toc-modellszelekció">Modellszelekció</a>

Ebben a portfolió fejezetben OLS regresszióval fogom megbecsülni a
budapesti lakások árát.

Az adatokat korábban szereztem az Ökonometria I. tárgyamon, eladó
budapesti lakások árát foglalja magában egyéb jellemzőiken kívül. A
mintavétel évét nem ismerem, azonban az átlagos négyzetméterár a
mintában 332 eFt, míg a [KSH
szerint](https://www.ksh.hu/stadat_files/lak/hu/lak0025.html) 2022-ben
az átlagos budapesti négyzetméterár előzetes adatok alapján 838 eFt.

# Kezdeti beállítások

## Használt library-k

``` r
library(tidyverse)
library(knitr)
library(psych)
library(corrplot)
library(ppcor)
library(car)
library(lmtest)
```

## Adatok beolvasása

Az adatok magyar .csv alapján a pontosvesszővel vannak elválasztva és a
tizedesjegyek sima vesszővel vannak jelölve. A beolvasott adatok első
hat megfigyelése a következőképpen néz ki:

``` r
df = read.csv("BP_Lakas.csv", sep = ";", dec = ",")
kable(head(df))
```

| KinArMFt | Terulet | Terasz | Szoba | Felszoba | Furdoszoba | Emelet | DeliTaj | Buda | Kerulet |
|---------:|--------:|-------:|------:|---------:|-----------:|-------:|--------:|-----:|--------:|
|     10.7 |      32 |      0 |     1 |        0 |          1 |      2 |       0 |    1 |       1 |
|     10.0 |      32 |      0 |     1 |        0 |          1 |      2 |       0 |    1 |       1 |
|     10.5 |      32 |      0 |     1 |        0 |          1 |      2 |       0 |    1 |       1 |
|     12.0 |      34 |      0 |     1 |        0 |          1 |      1 |       1 |    1 |       1 |
|     13.0 |      34 |      0 |     1 |        0 |          1 |      1 |       1 |    1 |       1 |
|     13.9 |      35 |      0 |     1 |        0 |          1 |      0 |       1 |    1 |       1 |

Látható, hogy 10 db változóm van, ezek a következők:

| Változó neve | Leírás                | Mértékegység  | Megjegyzés |
|:-------------|:----------------------|:--------------|:-----------|
| KinArMFt     | A lakás kínálati ára  | Millió Forint |            |
| Terulet      | A lakás alapterülete  | Négyzetméter  |            |
| Terasz       | A terasz alapterülete | Négyzetméter  |            |
| Szoba        | A szobák száma        | Darab         |            |
| Felszoba     | Félszobák száma       | Darab         |            |
| Furdoszoba   | Fürdőszobák száma     | Darab         |            |
| Emelet       | Hányadik emelet?      | Emelet        |            |
| Delitaj      | Déli tájolású-e?      | Logikai       | 1, ha igen |
| Buda         | Budai-e?              | Logikai       | 1, ha igen |
| Kerulet      | Kerület               | Kerület       | \[1;22\]   |

## Adatok átalakítása

Először is, átalakítom faktorrá(dummy változó) a *Delitaj*, *Buda* és
*Kerulet* változókat, mivel az első kettő logaikai, a harmadik pedig
kategorikus. A *Kerulet* változó annak ellenére kategorikus, hogy
számmal szerepel az adatbázisban, azonban szerepelhetnének római számmal
vagy névvel is. Abban az esetben pl. *21* helyett *Csepel* vagy *XXI*
szerepelne az adattáblában.

``` r
df[,c(8:10)] = lapply(df[,c(8:10)], factor)
```

# Leíró statisztika, adatvizualizációk

``` r
kable(describe(df[,-c(8:10)]))
```

|            | vars |    n |       mean |         sd | median |    trimmed |      mad | min | max | range |     skew |  kurtosis |        se |
|:-----------|-----:|-----:|-----------:|-----------:|-------:|-----------:|---------:|----:|----:|------:|---------:|----------:|----------:|
| KinArMFt   |    1 | 1406 | 26.4952347 | 19.6358286 |     21 | 22.9582593 | 12.00906 |   5 | 198 |   193 | 2.597186 | 10.580857 | 0.5236686 |
| Terulet    |    2 | 1406 | 76.9810242 | 42.1833066 |     68 | 71.2897780 | 29.65200 |  23 | 500 |   477 | 2.881761 | 17.851010 | 1.1249882 |
| Terasz     |    3 | 1406 |  5.3038407 | 12.1879247 |      0 |  2.5395204 |  0.00000 |   0 | 198 |   198 | 5.846486 | 58.182106 | 0.3250402 |
| Szoba      |    4 | 1406 |  2.5881935 |  1.2116176 |      2 |  2.4973357 |  1.48260 |   0 |  14 |    14 | 1.359284 |  6.584493 | 0.0323127 |
| Felszoba   |    5 | 1406 |  0.3947368 |  0.6233425 |      0 |  0.2886323 |  0.00000 |   0 |   5 |     5 | 1.662182 |  3.551691 | 0.0166239 |
| Furdoszoba |    6 | 1406 |  1.1237553 |  0.4205289 |      1 |  1.0328597 |  0.00000 |   0 |   5 |     5 | 2.535229 | 11.910064 | 0.0112151 |
| Emelet     |    7 | 1406 |  1.9103841 |  1.7282885 |      2 |  1.6989343 |  1.48260 |  -1 |  10 |    11 | 1.315364 |  2.673606 | 0.0460918 |

A leíró statisztikai mutatókat az egyes változók esetében nem
szándékozom külön külön értelmezni, azonban azt mindenképpen kiemelném,
hogy a lakásárak jobbra elnyúlóak és a normális eloszlásnál
csúcsosabbak, ezek az $\alpha_{3}$ és $\alpha_{4}$, valamint a medián és
átlag kapcsolatából látszanak. Emellett lakásárak eloszlását egy
hisztogrammon ábrázolva a következőtk kapjuk:

``` r
ggplot(df, aes(KinArMFt)) +
  geom_histogram(fill = "cyan3") +
  theme_minimal()+
  labs(x = "Kínálati ár(mFt)r", y = "Gyakoriság")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

A ábrán is látszik az erőteljes jobbra elnyúlás és a csúcsosság. Mivel
ez lesz az eredményváltozóm, érdemes lehet logaritmizálni, hogy normális
eloszlású legyen a válatozó.

``` r
ggplot(df, aes(log(KinArMFt)))+
  geom_histogram(fill = "cyan3")+
  theme_minimal()+
  labs(x = "Kínálati ár(mFt)", y = "Gyakoriság")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](ols_hun_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

A logaritmizált változó eloszlása már sokkal jobban hasonlít a normális
eloszláshoz.

A dummy változók vizualizására egy stacked barplotot alkalmaztam:

``` r
ggplot(df, aes(x = Buda, fill = DeliTaj))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("0" = "Pest", "1" = "Buda"))+
  scale_fill_discrete(labels = c("0" = "Nem déli", "1" = "Déli"))+
  theme_minimal()+
  labs(x = "Elhelyezkedés", y = "Arány", fill = "Tájolás")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Táblázatba rendezve:

``` r
dummy_table = table(df$DeliTaj,df$Buda)
rownames(dummy_table) = c("Nem déli", "Déli")
colnames(dummy_table) = c("Pest","Buda")
kable(addmargins(dummy_table))
```

|          | Pest | Buda |  Sum |
|:---------|-----:|-----:|-----:|
| Nem déli |  395 |  275 |  670 |
| Déli     |  369 |  367 |  736 |
| Sum      |  764 |  642 | 1406 |

Kevesebb budai eladó lakás van a mintában mint pesti, azonban köztük a
déli fekvésű ingatlanok aránya magasabb, mint a pesti ingatlanok
körében.

Az egyes eladó lakásokat kerületekre bontva szintén oszlopdiagrammon
ábrázoltam:

``` r
ggplot(df,aes(y = Kerulet, fill = Kerulet))+
  geom_bar(position = "dodge", stat = "count")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Ingatlanok száma", y = "Kerület")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Kiemelkedő számban találhatóak 2., 12. és 13. kerületi eladó lakások,
illetve a 3., 6. és 14. kerületi lakások is szép számmal vannak.

Az egyes kerületek kínálait árainak dobozábrái a következőképpen néznek
ki:

``` r
ggplot(df, aes(y = KinArMFt, x = Kerulet, col = Kerulet))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(y = "Kínálati ár (mFt)", x = "Kerület")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Az egyes kerületek medián lakására láthatóan eltérő, sok outlier van 2.,
11., 12., 14. kerületekben a nagyon magas lakásár tartományban.

A kínálati ár és a terület pontdiagrammját ábrázolva az elhelyezkedés
szerint a következőt kapjuk:

``` r
ggplot(df, aes(Terulet,KinArMFt, col = Buda))+
  geom_point()+
  stat_smooth(method = lm)+
  theme_minimal()+
  labs(y = "Kínálati ár (mFt)", x = "Alapterület (Nm)", col = "Elhelyezkedés")+
  scale_color_discrete(labels = c("0" = "Pest", "1" = "Buda"))
```

![](ols_hun_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Látszik, hogy a budai lakások árai az alapterülethez képest meredekebben
emelkednek, mint a pestieké. Érdemes lehet esetleg interakciót
feltételezni az alapterület és az elhelyezkedés között a későbbi
modellben.

# Korreláció

Az adatok együttmozgásáról készítettem először korrelogrammot:

``` r
corrplot(cor(df[,1:7]), addCoef.col = "black", method = "color", col = COL2("BrBG"), diag = F, type = "lower")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

A kínálati árral erősen korrelál pozitív irányba a lakás alapterülete és
a szobák száma, illetve közepesen, szintén pozitív irányban a terasz
mérete és a fürdőszobák száma. Az emelet és a félszobák számának
korrelációja a kínálati árral szinte 0.  
Nem meglepő módon erősen korrelál a terület a szobák számával, pozitív
irányban.

A parciális korreláció ábrázolva:

``` r
corrplot(pcor(df[,1:7])$estimate, addCoef.col = "black", method = "color", col = COL2("BrBG"), diag = F, type = "lower")
```

![](ols_hun_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Az egyéb változók hatásától megtisztított korrelációkat ábrázolva
látszik, hogy a terület még így is pozitívan, közepesen korrelál a
kínálati árral és a szobák számával. A szobák száma szintén közepesen,
pozitívan korrelál a félszobák számával. Érdekes módon ebben az esetben
a parciális korreláció a terület és a félszoba között gyengén negatív.

# Modellépítés

Már az adatvizualizációknál szembe ötlött, hogy érdemes lenne
logaritmizálni az eredményváltozót, ezzel kezelve a jobbra elnyúlást.:  
Ha nem logaritmizálnám az eredményváltozót, akkor a becsült egyenlet a
következőképpen nézne ki:  
$\hat{Y} = \beta_{0} + \beta_{1}\times X_{1} + \beta_{2}\times X_{2} + \dots + \beta_{n}\times X_{n}$:  
Amennyiben azonban a logaritmizálom az eredményváltozót, úgy a becsült
egyenlet a következőképpen nézne ki:  
$log(\hat{Y})= \beta_{0} + \beta_{1}\times X_{1} + \beta_{2}\times X_{2} + \dots + \beta_{n}\times X_{n}$,
amiből ha kifejezzük $\hat{Y}$-t, akkor az kapjuk, hogy:  
$\hat{Y} = e^{\beta_{0} + \beta_{1}\times X_{1} + \beta_{2}\times X_{2} + \dots + \beta_{n}\times X_{n}}= e^{\beta_{0}} \times e^{\beta_{1}\times X_{1}} \times e^{\beta_{2}\times X_{2}} \times \dots \times e^{\beta_{n}\times X_{n}}$:  
Látható, hogy az egyes magyarázó változók hatásai marginális hatások
lesznek, azaz egy egységnyi $X_{n}$ változás hatására
$\beta_{n}$-szeresével változik $\hat{Y}$. Például ha $\beta_{n} = 0,1$,
akkor egységnyi $X_{n}$ növekedés hatására $\hat{Y}$ értéke 10%-kal nő.

## Alapmodell

Az alapmodellben minden változó szerepel, az eredményváltozó pedig
logaritmizálva van.

``` r
alapmodell = lm(log(KinArMFt) ~ Terulet + Terasz + Szoba + Felszoba + Furdoszoba + Emelet + DeliTaj + Buda + Kerulet,df)
summary(alapmodell)
```

    ## 
    ## Call:
    ## lm(formula = log(KinArMFt) ~ Terulet + Terasz + Szoba + Felszoba + 
    ##     Furdoszoba + Emelet + DeliTaj + Buda + Kerulet, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.26934 -0.17954 -0.00606  0.17294  0.90029 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.9621604  0.0923880  21.238  < 2e-16 ***
    ## Terulet      0.0071705  0.0003457  20.740  < 2e-16 ***
    ## Terasz       0.0072086  0.0006968  10.345  < 2e-16 ***
    ## Szoba        0.1167159  0.0116078  10.055  < 2e-16 ***
    ## Felszoba     0.0005011  0.0137356   0.036 0.970901    
    ## Furdoszoba  -0.0317617  0.0225174  -1.411 0.158605    
    ## Emelet       0.0143593  0.0045026   3.189 0.001459 ** 
    ## DeliTaj1     0.0540916  0.0152216   3.554 0.000393 ***
    ## Buda1        0.3900818  0.0953391   4.092 4.53e-05 ***
    ## Kerulet2     0.0935029  0.0400189   2.336 0.019610 *  
    ## Kerulet3    -0.0739431  0.0436161  -1.695 0.090242 .  
    ## Kerulet4    -0.0705051  0.1027084  -0.686 0.492539    
    ## Kerulet5     0.5057620  0.0952182   5.312 1.27e-07 ***
    ## Kerulet6     0.1165212  0.0925376   1.259 0.208180    
    ## Kerulet7    -0.0622972  0.0966645  -0.644 0.519379    
    ## Kerulet8    -0.1838889  0.1018805  -1.805 0.071301 .  
    ## Kerulet9     0.0407383  0.0963088   0.423 0.672364    
    ## Kerulet10   -0.2161953  0.1525879  -1.417 0.156751    
    ## Kerulet11   -0.2034248  0.0461336  -4.409 1.12e-05 ***
    ## Kerulet12   -0.0782221  0.0403824  -1.937 0.052945 .  
    ## Kerulet13    0.1075107  0.0911766   1.179 0.238543    
    ## Kerulet14    0.0535830  0.0921621   0.581 0.561066    
    ## Kerulet15   -0.0337206  0.0985716  -0.342 0.732334    
    ## Kerulet16   -0.0388148  0.1319584  -0.294 0.768692    
    ## Kerulet17   -0.2188235  0.1521282  -1.438 0.150543    
    ## Kerulet18   -0.0483528  0.1014221  -0.477 0.633617    
    ## Kerulet19   -0.0690719  0.1523465  -0.453 0.650342    
    ## Kerulet20   -0.0972550  0.1642217  -0.592 0.553802    
    ## Kerulet21   -0.2051189  0.2152071  -0.953 0.340695    
    ## Kerulet22           NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2762 on 1377 degrees of freedom
    ## Multiple R-squared:  0.7957, Adjusted R-squared:  0.7916 
    ## F-statistic: 191.6 on 28 and 1377 DF,  p-value: < 2.2e-16

A koefficienseket ebben az esetben még nem értelmezem, azonban az
R<sup>2</sup> szerint a modell magyarázó ereje 79,75% és korrigált
R<sup>2</sup> biztatóan magas. Látszik azonban, hogy nem minden
együttható bétája szignifikáns, ezeket az együtthatókat a következőkben
fogom megvizsgálni és egy esetében pedig NA-t ír az R. Ez azért lehet,
mert valószínüleg nem ad hozzá a modellhez semmi magyarázóerőt a 22.
kerület dummy.

A kerületeknél a dummy kódolás miatt az első kerületet vette alapul és a
béták értelmezésénél ahhoz vannak hasonlítva a kerületek. Látható, hogy
sok kerület nem tér el szignifikánsan az első kerülettől és mivel ez
21(=22-1) új változó bevonását jelenti, ezért az információs kritériumok
alapján fogom eldönteni, hogy jobb-e ha kiveszem őket belőle.

## Modellszelekció

Mindenek előtt megvizsgálom, hogy van-e multikollinearitás a modellben.

``` r
vif(alapmodell)
```

    ## Error in vif.default(alapmodell): there are aliased coefficients in the model

Nem fut le a VIF függvény, mégpedig azért, mert tökéletes
multikollineraitás van a modellben. Ez megmagyarázza a 22. kerület NA
értékét is. Megpróbálom a kerületek nélkül is megnézni a VIF mutatót.

``` r
vif(lm(log(KinArMFt) ~ Terulet + Terasz + Szoba + Felszoba + Furdoszoba + Emelet + DeliTaj + Buda,df))
```

    ##    Terulet     Terasz      Szoba   Felszoba Furdoszoba     Emelet    DeliTaj 
    ##   3.682941   1.261302   3.552680   1.270590   1.633121   1.037153   1.038851 
    ##       Buda 
    ##   1.113975

Így már lefut a VIF, azonban van a terület és a szobák számánál így is
zavaró mutlikollinearitás van.

``` r
szukitett_modell1 = lm(log(KinArMFt) ~ Terulet + Terasz + Szoba + Felszoba + Furdoszoba + Emelet + DeliTaj + Buda,df)

IC = AIC(alapmodell, szukitett_modell1)
IC$BIC = BIC(alapmodell, szukitett_modell1)$BIC

kable(IC)
```

|                   |  df |      AIC |      BIC |
|:------------------|----:|---------:|---------:|
| alapmodell        |  30 | 402.3455 | 559.8007 |
| szukitett_modell1 |  10 | 655.7834 | 708.2685 |

Mind az AIC, mind a BIC jobban preferálja, ha a kerületek szerepelnek a
magyarázó változók között.
