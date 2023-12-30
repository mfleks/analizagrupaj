library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 
library(janitor)
library(naniar)
library(mice)
library(editrules)
library(DMwR2)
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("janitor")
install.packages("naniar")
install.packages("mice")
install.packages("editrules")
install.packages("DMwR2")


rowery <- clean_names(sklep_rowerowy) #usuniecie spacji itp.
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}  #funkcja, ktora wstawia w puste miejsca wartosci NULL, jezeli ich tam nie ma
sapply(rowery, is.special) 
sum(complete.cases(rowery)) #952 pełnych wierszy, czyli mamy 48 wierszy, w ktorych sa braki
manyNAs(rowery) #689 wiersz zawiera najwiecej brakow, bo az 3, wiec moze nalezy rozwazyc jego usuniecie


miss_var_summary(rowery) #widzimy, że braki wystepuja jedynie dla zmiennych gender (11), cars(9),children (8), age(8),marital_status(7)
#income(6) i home_owner(4). Dla pozostalych zmiennych nie wystepuja braki
md.pattern(rowery) #wykres do zilustrowania brakow kazdej zmiennej
which(is.na(rowery$gender)) #4, 155, 336, 602, 689, 696, 868, 909, 952, 974, 998- numery wierszy, w ktorych sa braki dla zmiennej gender
which(is.na(rowery$cars)) #13, 197, 203, 352, 449, 512, 562, 616, 934-numery wierszy, w ktorych sa braki dla zmiennej cars
which(is.na(rowery$children)) #118, 218, 387, 550, 639, 689, 806, 961- numery wierszy, w ktorych sa braki dla zmiennej children
which(is.na(rowery$age)) #10,  99, 226, 372, 555, 689, 771, 987-numery wierszy, w ktorych sa braki dla zmiennej age
which(is.na(rowery$marital_status)) #9,  28,  50,  99, 151, 235, 302-numery wierszy, w ktorych sa braki dla zmiennej marital_status
which(is.na(rowery$income)) #10, 111, 192, 302, 442, 510-numery wierszy, w ktorych sa braki dla zmiennej income
which(is.na(rowery$home_owner)) #7, 366, 647, 944-numery wierszy, w ktorych sa braki dla zmiennej home_owner

summary(rowery) #przez wywolanie podstawowych statystyk sprawdzam, czy zmienne maja odpowiedni format, sa wiarygodne itp.

# tworze regule, ze wiek musi byc wiekszy od 0, plec moze byc tylko "kobieta" albo "mezyczyzna", dochod nie moze byc ujemny, 
#podobnie jak liczba posiadanych samochodow czy dzieci
RULE <- editset(c("age > 0","gender %in% c('Female','Male')"
                  , "income >= 0", "children >= 0", "cars >= 0"))
RULE

summary(violatedEdits(RULE, rowery)) #widzimy, ze naruszenie jednej z regul westepuje w 37 wierszach, a naruszenie jednoczesnie 2 albo
#3 regul wystepuje w jednym z wierszy

rowery[localizeErrors(RULE, rowery)$adapt] <- NA #uzupelniamy wiersze, gdzie jest niespelniona regula wartoscia NA
sum(complete.cases(rowery)) #dalej mamy 952 puste wiersze, czyli niespelnienie regul wynikalo, ze wartosc NA byla tam juz wczesniej

#Po zlikalizowaniu bledow i sprawdzeniu wiarygodnosci danych mozemy przejsc do uzupelniania badz tez wypelniania brakow

