library(dplyr) 
library(ggplot2)
library(summarytools)
install.packages("rmdformats")
library(rmdformats)
install.packages("validate")
library(validate)
install.packages("validatetools")
library(validatetools)
library(dcmodify)
library(errorlocate)
library(deductive)
library(simputation)
library(lumberjack)
library(ISLR)
library(dlookr)
library(xts)
library(quantmod)
library(ROCR)
library(DMwR)
library(Information)
library(scorecard)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 
library(janitor)
library(naniar)
library(mice)
library(editrules)
library(DMwR2)
library(outliers)
library(moments)
library(classInt)
library(corrplot)
{r}
#Na poczatku wczytujemy zbior danych
rowery<-read_excel("sklep_rowerowy.xlsx")
rowery

```{r}
#Na poczatku wczytujemy zbior danych
rowery<-read_excel("sklep_rowerowy.xlsx")
rowery
```

# **SKLEP ROWEROWY**

Zbiór danych zawiera informacje na temat klientów sklepu rowerowego. Z jego pomocą możemy uzyskać informacje na temat wieku, dochodu, płci, liczby posiadanych dzieci czy regionu. Celem projektu jest przeprowadzenie kompleksowej analizy danych, która obejmować będzie kilka kluczowych etapów, a mianowicie: czyszczenie danych, wizualizację, analizę opisową i wnioskowanie statystyczne. 


## I ETAP - CZYSZCZENIE DANYCH

### 1. Przegląd danych

Obserwujemy m.in spacje na nazwach kolumn naszych zmiennych. Aby się ich pozbyć korzystamy z funkcji clean_names z pakietu 'janitor', która służy do przekształcania nazw kolumn w ramce danych w formę bardziej czytelną i zgodą z konwencją.

```{r}
head(rowery) #obserwujemy np.spacje w nazwach kolumn, aby sie ich pozbyc uzywamy funkcji clean_names
rowery <- clean_names(rowery) 
```

#### 1. Sprawdzenie występowania brakujących obserwacji NA

Poniżej przeprowadzona została analiza wartości brakujących NA dla poszczególnych zmiennych. Wskazano zarówno liczbę wierszy, w których występują braki, jak i konkretne ich numery.W ten sposób wiemy np., że zmienna "home_owner" ma 4 braki i występują one w wierszach: 7, 366, 647 i 944. Przedstawiono również wizualizację wartości brakujących.   

```{r}
sum(complete.cases(rowery)) #952 pełnych wierszy, czyli mamy 48 wierszy, w ktorych sa braki
manyNAs(rowery) #689 wiersz zawiera najwiecej brakow
sum(is.na(rowery)) #w sumie wystepuja 53 braki
miss_var_summary(rowery) #widzimy, że braki wystepuja jedynie dla zmiennych gender (11), cars(9),children (8), age(8), marital_status(7), income(6) i home_owner(4). Pozostale zmienne nie posiadaja zadnych wartosci brakujacych
rowery %>% 
  miss_case_table() #mamy 44 wiersze, w ktorych brak jest jednej wartosci, 3 wiersz z 2 brakami i jeden wiersz z 3 wartosciami NA
md.pattern(rowery) #wykres do zilustrowania brakow kazdej zmiennej
which(is.na(rowery$gender)) #4, 155, 336, 602, 689, 696, 868, 909, 952, 974, 998- numery wierszy, w ktorych sa braki dla zmiennej gender
which(is.na(rowery$cars)) #13, 197, 203, 352, 449, 512, 562, 616, 934-numery wierszy, w ktorych sa braki dla zmiennej cars
which(is.na(rowery$children)) #118, 218, 387, 550, 639, 689, 806, 961- numery wierszy, w ktorych sa braki dla zmiennej children
which(is.na(rowery$age)) #10,  99, 226, 372, 555, 689, 771, 987-numery wierszy, w ktorych sa braki dla zmiennej age
which(is.na(rowery$marital_status)) #9,  28,  50,  99, 151, 235, 302-numery wierszy, w ktorych sa braki dla zmiennej marital_status
which(is.na(rowery$income)) #10, 111, 192, 302, 442, 510-numery wierszy, w ktorych sa braki dla zmiennej income
which(is.na(rowery$home_owner)) #7, 366, 647, 944-numery wierszy, w ktorych sa braki dla zmiennej home_owner
```

#### 2. Sprawdzenie typów danych w każdej kolumnie

```{r}
data_class <- data.frame(class = sapply(rowery, class))
data_class
```

#### 3. Sprawdzenie wiarygodności danych

Poprzez wywołanie podstawowych statystyk możemy sprawdzić, czy nasze dane wydają się być wiarygodne, tzn. czy średnia ilość posiadanych dzieci, czy samochodów wydaje się być prawdopodobna. Ponadto użycie 'view(dfSummary(dane))' dostarcza nam wielu cennych informacji m.in.:
  -widzimy, że każdy klient posiada unikalne id,
-otrzymujemy podsumowanie braków obserwacji dla każdej zmiennej,
-dla zmiennych jakościowych widzimy możliwe kategorie wyboru wraz z informacją o częstości jej wystąpienia.

```{r}
summary(rowery)
view(dfSummary(rowery))
descr(rowery)
```

#### 4. Sprawdzanie spełnienia pewnych reguł dla zbioru danych

Posiadając już podstawową wiedzę na temat naszych danych, chcemy sprawdzić kilka podstawowych reguł:
  -czy zmienna wiek na pewno wszędzie przyjmuje wartości dodatnie,
-czy liczba posidanych dzieci nie jest nigdzie ujemna,
-czy liczba posiadanych samochodów nie jest ujemna,
-czy płeć przyjmuje jedną z dwóch kategorii 'kobieta' i 'mężczyna'

```{r}
rules <- validator(age > 0, gender %in% c('Female','Male')
                   , income >= 0, children >= 0, cars >= 0)
cf <- confront(rowery, rules, key="id")
summary(cf)
barplot(cf, main="rowery") #wizualizacja spełnienia reguły
#as.data.frame(cf) %>% head()

#Inny sposob
RULE <- editset(c("age > 0","gender %in% c('Female','Male')"
                  , "income >= 0", "children >= 0", "cars >= 0"))
RULE

summary(violatedEdits(RULE, rowery))
#rowery[localizeErrors(RULE, rowery)$adapt] <- NA -> w razie niespelnienia ktores z regul wartosci zmieniamy na NA

#Reguły w naszym zbiorze danych są spełnione!

```

### 2. Imputacja danych

Jak wynikało z analiz przeprowadzonych powyżej, w naszym zbiorze danych zlokalizowano 53 braki danych. Podjęto decyzję o nieusuwaniu żadnego z wierszy i uzupełnieniu wartości NA za pomocą różnych metod. 

#### 1. Imputacja-zmienne ilościowe

Braki w zmiennych dotyczących dochodu, wieku, liczby posidanych dzieci i samochodów wypełnione zostaną za pomocą średniej. Ze względu na specyfikę danych age, children i cars, wartości te nie powinny mieć żadnych miejsc po przecinku, gdyż nie możemy posiadać np. 1.5 dziecka. 

```{r}
rowery %>% filter(is.na(income)) %>% head
rowery[is.na(rowery$income), "income"] <- mean(rowery$income, na.rm = T)

# Inny sposob
#dochod<-imputate_na(rowery, income, method = "mean")
#summary(dochod)
#plot(dochod)

rowery %>% filter(is.na(age)) %>% head
rowery[is.na(rowery$age), "age"] <- round(mean(rowery$age, na.rm = T), digits=0)

rowery %>% filter(is.na(cars)) %>% head
rowery[is.na(rowery$cars), "cars"] <- round(mean(rowery$cars, na.rm = T), digits=0)

rowery %>% filter(is.na(children)) %>% head
rowery[is.na(rowery$children), "children"] <- round(mean(rowery$children, na.rm = T), digits=0)
```

#### 2. Imputacja-zmienne jakościowe

Po uzupełnieniu braków w zmiennych ilościowych, przystępujemy teraz do korekty brakujących danych w przypadku zmiennych jakościowych. Ponieważ nasze zmienne jakościowe zawierają jedynie dwie kategorie, planujemy zidentyfikować, która z tych kategorii jest dominująca. Następnie, braki danych w tych zmiennych zostaną wypełnione wartościami dominującymi, aby zachować spójność i ułatwić analizę danych.

```{r}
unique(rowery$home_owner) # dwie kategorie- tak i nie
unique(rowery$marital_status) # dwie kategorie- zamezny i singiel
unique(rowery$gender) # dwie kategorie- kobieta i mezczyzna

mice_plot <-aggr(rowery, col=c('navyblue', 'yellow'),numbers=TRUE, sortVars=TRUE, labels=names(rowery), cex.axis=7, ylab=c("Histogram of missing data","Pattern")) #wykres obrazujacy braki danych

#uzupelnianie danych dominanta
rowery[is.na(rowery$home_owner), "home_owner"] <- "Yes" #68,5% klientow posiada dom, jest to wiekszosc, wiec braki w danych uzupelniamy wartosci 'Yes'
rowery[is.na(rowery$marital_status), "marital_status"] <- "Married" #53,9% klientow posiada meza/zone, wiec braki w danych uzupelniamy wartoscia 'Married'
rowery[is.na(rowery$gender), "gender"] <- "Male" #50,6% klientow to mezczyznki, wiec braki w danych uzupelniamy wartoscia 'Male'

view(dfSummary(rowery)) #Wywolujemy tabelke podsumowujaca nasze dane. Widzimy, ze wszystkie dane zostaly uzupelnione.

```

### 3. Obserwacje odstające 

Sprawdzanie zbioru danych pod kątem wartości odstających jest kluczowe dla utrzymania jakości analizy danych i poprawnego zrozumienia badanego zjawiska. W przypadku identyfikacji wartości odstających, istnieją różne metody ich obsługi, takie jak usuwanie, transformacja, czy stosowanie bardziej zaawansowanych technik modelowania.
Przechodzimy więc do sprawdzenia naszego zbioru danych pod względem występowania wartości odstających.

#### 1. Zlokalizowanie wartości odstających

```{r}
#mozemy stworzyc funkcje, ktora bedzie wykrywac odstajace obserwacje
find_outliers <- function(rowery, k = 1.5) {
  quantiles <- quantile(rowery, c(0.25, 0.5, 0.75))
  diff <- k * (quantiles[3] - quantiles[1])
  lb <- quantiles[1] - diff 
  ub <- quantiles[3] + diff
  
  is_outlier <- function(el) {
    el < lb || ub < el  
  }}
#Inne metody
out <-boxplot.stats(rowery$income)$out #mamy 10 wartosci odstajacych dla income
test <- grubbs.test(rowery$income)
test #test Grubba'a potwierdza, ze najwyzsza wartosc 170 000 jest odstajaca
test1 <- grubbs.test(rowery$income, opposite = TRUE)
test1 #najmniejsza wartosc nie jest odstajaca
boxplot(rowery$income, col = "blue",
        ylab = "income",
        main = "Boxplot of income")
mtext(paste("Outliers: ", paste(out, collapse = ", "))) #mozemy zwizualizowac wartosci odstajacae na wykresie wraz z opisem, ktore z nich sa odstajace

boxplot.stats(rowery$age)$out #mamy 4 wartosci odstajace dla age
test <- grubbs.test(rowery$age)
test #test potwierdza, ze najwyzsza wartosc 89 jest odstajaca
```

#### 4. Przekształcenie wartości odstających

Zdecydowano się przekształcić wartości odstające dla zmiennych "age" i "income" za pomocą metody capping.Metoda capping polega na ustaleniu górnej (max) i dolnej (min) granicy wartości dla danej zmiennej, a następnie przypisanie wszystkim wartościom przekraczającym te granice wartości skrajnych.  

```{r}
#income
qnt <- quantile(rowery$income, probs=c(.25, .75), na.rm = T)
caps <- quantile(rowery$income, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(rowery$income, na.rm = T)
rowery$income[rowery$income < (qnt[1] - H)] <- caps[1]
rowery$income[rowery$income > (qnt[2] + H)] <- caps[2]
boxplot.stats(rowery$income)$out #brak wartosci odstajacych dla income

#age
qnt <- quantile(rowery$age, probs=c(.25, .75), na.rm = T)
caps <- quantile(rowery$age, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(rowery$age, na.rm = T)
rowery$age[rowery$age < (qnt[1] - H)] <- caps[1]
rowery$age[rowery$age > (qnt[2] + H)] <- caps[2]
boxplot.stats(rowery$age)$out #brak wartosci odstajacych dla age

```
## Korelacja zmiennych ilościowych
# Poniżej została obliczona korelacja zmiennych ilościowych
```{r}
cor((rowery[,c(4,5,9,12)]), method="pearson")
corrplot(cor(rowery[,c(4,5,9,12)]), method = "number", type = "upper", diag =FALSE)
corr_matrix<-cor(rowery[,c(4,5,9,12)])
corrplot(corr_matrix, method="color")