#Instalacja oraz wczytanie niezbędnych pakietów 
install.packages("readxl") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("visdat")
install.packages("assertive.base")
install.packages("janitor")
library(readxl)
library(dplyr)
library(ggplot2)
library(visdat)
library(assertive.base)
library(janitor)
#Weryfikacja tego, czy w folderze zawierającym plik R znajduje się plik z wymaganymi danymi
dir()
#Wgranie danych w formacie xlsx
bazarowery<-read_excel("sklep_rowerowy.xlsx") 
bazarowery
#Zmiana nazw kolumn
#Przy tworzeniu baz danych do wykorzystania w programie R zalecane jest niewystępowanie spacji w nazwach kolumn oraz niewykorzystywanie dużych liter. W wyeliminowaniu występowania tych zjawisk pomaga pakiet janitor wprowadzający funkcję clean_names.
bazarowery<-clean_names(bazarowery)
#W wyniku wykorzystania polecenia clean_names nazwy kolumn zostały ujednolicone, od teraz nie zawierają one ani spacji, ani dużych liter.
#Etap 1: Czyszczenie danych
#1.1 Wizualizacja brakujących danych- pakiet Visdat
vis_miss(bazarowery)
#1.2 Identyfikacja klas zmiennych
#Wykres wskazuje na to, że w przypadku kolumn id, education, occupation, home_owner, commute_distance, region oraz purchased_bike nie występują braki danych. Należy to jednak zweryfikować, ponieważ przy otrzymanym wykresie nie zostało zastosowane miejsce po przecinku. Przy weryfikacji pomocne będzie zidentyfikowanie typów danych dotyzących wszystkich 13 zmiennych za pomocą funkcji class, określające klasę danych w poszczególnych kolumnach.
class(bazarowery$id)
class(bazarowery$marital_status)
class(bazarowery$gender)
class(bazarowery$income)
class(bazarowery$children)
class(bazarowery$education)
class(bazarowery$occupation)
class(bazarowery$home_owner)
class(bazarowery$cars)
class(bazarowery$commute_distance)
class(bazarowery$region)
class(bazarowery$age)
class(bazarowery$purchased_bike)
#Podsumowując efekt wykorzystania funkcji class można wykazać, że dla zmiennych id, income, children, cars oraz age wartości są numeryczne, natomiast dla pozostałych zmiennych ich wartości mają wartości tekstowe.
#Dla każdej zmiennej zostanie zweryfikowane występowanie braków danych.
#Dla zmiennych numerycznych w celu weryfikacji występowania braków danych oraz sprawdzenia ich położenia zostanie wykorzystana metoda sprowadzenia kolumn zawierających zmienne numeryczne do postaci factor, a następnie wykorzystanie połączenia formuł which oraz is.na dla każdego z utworzonych factorów, w ccelu określenia dokładnego położenia braków danych.
idfactor<-as.factor(bazarowery$id)
idfactor
is.na(idfactor)
#Dla kolumny id nie stwierdzono występowania braków danych.
incomefactor<-as.factor(bazarowery$income)
incomefactor
which(is.na(incomefactor))
#Dla kolumny income braki danych odnotowano w 10, 111, 192, 302, 442 oraz 510 wierszu.
childrenfactor<-as.factor(bazarowery$children)
childrenfactor
which(is.na(childrenfactor))
#Dla kolumny children braki danych odnotowano w 118, 218, 387, 550, 639, 689, 806 oraz 961 wierszu.
carsfactor<-as.factor(bazarowery$cars)
carsfactor
which(is.na(carsfactor))
#Dla kolumny cars braki danych odnotowano w 13, 197, 203, 352, 449, 512, 562, 616 oraz 934 wierszu.
agefactor<-as.factor(bazarowery$age)
agefactor
which(is.na(agefactor))
#Dla kolumny age braki danych odnotowano w 10, 99, 226, 372, 555, 689, 771 oraz 987 wierszu.
#Dla kolumn zawierających dane liczbowe kolejnym krokiem będzie sprawdzenie tego, czy zawarte w nich dane liczbowe są realistyczne. Dla zmiennej ID zostanie sprawdzona średnia, mediana, wartość minimalna oraz wartość maksymalna.
min(bazarowery$id)
max(bazarowery$id)
mean(bazarowery$id)
median(bazarowery$id)
#Kolumna id zawiera pięciocyfrowe numery identyfikujące każdą ankietowaną osobę, nie mogły one się zatem powtarzać. Otrzymano wartość minimalną równą 11000, wartość maksymalną równą 29447, średnią równą 19966 oraz medianę równą 19744. Wyniki te nie wskazują na nieprawidłowość żadnej z obserwacji.
bazarowery%>%
filter(income<=0)
#Dla kolumny income nie odnotowano w żadnym przypadku wartości dochodów mniejszej niż 0, której występowanie wskazywałoby na wystąpienie błędu.
bazarowery%>%
filter(income>=0) %>%
summarize(mean(income), median(income), min(income), max(income))
#Z wyników dla kolumny income wynika, że najniższy odnotowany dochód wyniósł 10000 jednostek, najwyższy 170000 jednostek, mediana dochodów wyniosła 60000 jednostek, a ich średnia około 56268 jednostek. Wyniki te nie wskazują na nieprawidłowość danych.
#Dwie następne zmienne- children oraz cars- określają ilość posiadanych przez badane jednostki dzieci oraz samochodów. W przypadku tych zmiennych zostanie zbadana ilość wystąpień wszystkich możliwych wartości tych zmiennych.
bazarowery%>%
count(children)
#W 274 przypadkach ilość dzieci wyniosła 0, w 169 przypadkach wyniosła 1, w 209 przypadkach wyniosła ona 2, 133 razy odnotowano wartość wynoszącą 3, 126 razy odnotowano posiadanie 4 dzieci, a 81 razy respondent posiadał 5 dzieci. Z wyjątkiem braków danych, które odnotowano 8 razy, wyniki można uznać za wiarygodne oraz niewymagające korekt.
bazarowery%>%
filter(children!=is.na(children)) %>%
summarize(round(mean(children), digits=0))
#Po zaokrągleniu do 0 miejsc po przecinku średnia ilość dzieci dla prawidłowych obserwacji wynosi 3.
bazarowery%>%
count(cars)
#238 respondentów nie posiadało samochodu, 267 z nich posiadało 1 samochód, 342 osoby posiadały 2 samochody, 85 osób posiadało 3 samochody, a 59 respondentów wskazało, że posiada 4 samochody. Braki danych odnotowano w 9 przypadkach. Wyniki można uznać za niewymagające korekt z wyjątkiem brakujących obserwacji.
bazarowery%>%
filter(cars!=is.na(cars)) %>%
summarize(round(mean(cars), digits=0))
#Po zaokrągleniu do 0 miejsc po przecinku średnia ilość samochodów dla prawidłowych obserwacji wynosi 2.
#Z uwagi na to, że w przypadku kolumny age określającej wiek respondenta różnorodność odpowiedzi jest znacznie większa, zostanie ona poddana podobnej analizie co kolumny id oraz income. 
agefactor<-as.factor(bazarowery$age)
which(is.na(agefactor))
#Dla kolumny age braki danych odnotowano w 10, 99, 226, 372, 555, 689, 771 oraz 987 wierszu.
bazarowery%>%
filter(age>=0)%>%
summarize(round(mean(age), digits=0),median(age),max(age),min(age))
#Dla obserwacji zawierających dane liczbowe odnotowano średnią wynoszącą przy zaokrągleniu do 0 miejsc po przecinku 44 lata, medianę wynoszącą 43 lata, najniższy wiek wynoszący 89 lat oraz najniższy wiek wynoszący 25 lat.
bazarowery%>%
select(income, age) %>%
arrange(desc(age)) %>%
head(20)
#Przy wynikach dotyczących kolumny age warto zwrócić uwagę na bardzo wysoki maksymalny wiek- jedna osoba posiadała dochód wynoszący 40000 jednostek rocznie w wieku 89 lat. Wynik ten można uznać za pewną anomalię- druga najmłodsza badana osoba miała 80 lat.
#Kolejnym etapem będzie weryfikacja wartości zmiennych tekstowych (typu "character"). Są to zmienne marital_status, gender, education, occupation, home_owner, commute_distance, region oraz purchased_bike. W przypadku każdej z nich zostanie zbadana ilość obserwacji o różnych wartościach tych zmiennych.
bazarowery%>%
count(marital_status)
#535 respondentów określiło siebie jako osoby posiadające męża/żonę, 458 osoby określiły siebie jako niezamężne, a w 7 przypadkach nie uzyskano odpowiedzi.
bazarowery%>%
count(gender)
#Wśród badanych osób wystąpiło 489 kobiet oraz 500 mężczyzn. W 11 przypadkach nie otrzymano odpowiedzi na pytanie dotyczące płci.
bazarowery%>%
count(education)
#W przypadku zmiennej dotyczącej poziomu wykształcenia nie odnotowano braków danych.
bazarowery%>%
count(occupation)
#W przypadku zmiennej dotyczącej wykonywanej pracy nie odnotowano braków danych.
bazarowery%>%
count(home_owner)
#W przypadku zmiennej określającej to, czy osoba ankietowana jest właścicielem swojego miejsca zamieszkania, 314 osoby odpowiedziały "Nie", 682 osób odpowiedziało "Tak", a w 4 przypadkach wystąpił brak danych.
bazarowery%>%
count(commute_distance)
#W przypadku zmiennej określającej dystans pomiędzy miejscem pracy a domem nie wystąpiły braki danych.
bazarowery%>%
count(region)
#W przypadku zmiennej określającej region osoby ankietowanej nie wystąpiły braki danych.
bazarowery%>%
count(purchased_bike)
#W przypadku zmiennej określającej to, czy osoba ankietowana zakupiła rower, nie wystapiły braki danych. 519 osób odpowiedziało twierdząco, a 481 przecząco.
#Podsumowując, zmiennymi liczbowymi, w przypadku których wystąpiły braki danych, były zmienne income, children, cars oraz age, natomiast zmiennymi tekstowymi, w przypadku których wystąpiła taka sytuacja, były zmienne marital_status, gender home_owner oraz purchased_bike. 
#Następnym etapem pracy z danymi będzie doprowadzenie do sytuacji, w której poddawane analizie dataframe bazarowery będzie pozbawione obserwacji brakujących oraz odstających.