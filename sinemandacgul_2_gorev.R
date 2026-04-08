# R'de Temel Fonksiyonlar
getwd()

# R'de basit bir matematiksel kod temel fonksiyonlar ile yapilabilir.
kup_alma <- function(x) {
  return(x^3)
}
print(kup_alma(8)) 

topla <- function(x,y) {
  return(x+y)
}
print(topla(1500,1200))

isim <- "Veri Madenciligi"
toupper(isim)   # Buyuk harfe cevirir
substr(isim, 1, 4) # Ilk 4 karakteri alir

# gerekli paketleri yukleme
# install.packages("tidyverse") # verinin aktarildigi, duzenlendigi, donusturuldugu bir pakettir.
# install.packages("zoo")       # eksik verileri doldurabilecegimiz bir pakettir.
library(tidyverse)

data(mtcars) # R yazilimindan bir veri seti kullanmak istedim. 
veri_sinem <- mtcars # veri dosyasini adima tanimladim.
head(veri_sinem)     # Ilk 6 satiri gormek istedim
dim(veri_sinem)      # Kac satir, kac sutun var ogrenmek istedim
summary(veri_sinem)  # Istatistiksel ozeti istedigim fonksiyon

# Veride bosluk olusturma
veri_sinem$mpg[c(2, 5)] <- NA

# Eksik verileri ortalama ile doldur (na.rm = TRUE)
ortalama_mpg <- mean(veri_sinem$mpg, na.rm = TRUE)
veri_sinem$mpg[is.na(veri_sinem$mpg)] <- ortalama_mpg

# veri turunu degistirme: nicel (cyl) verisini kategorik yaparak gruplandirma yaptim
veri_sinem$cyl <- as.factor(veri_sinem$cyl)

# scale() fonksiyonu ile z puani hesapladim
veri_sinem$hp_scaled <- scale(veri_sinem$hp)

# Urettigim z puanlari yeni sekmede gormek istedim.
View(veri_sinem)

# Yakit tuketimi (mpg) 15'ten kucuk olanlari secip filtrelemek istedim
ekonomik_arabalar <- subset(veri_sinem, mpg < 15)

# Bazi ozelliklerine gore sutun sectim
net_listem <- ekonomik_arabalar[, c("mpg", "cyl", "hp", "hp_scaled")]

# Sectigim sutunlarla yeni bir liste olusturdum, bu arada hp degerini 100'e bolerek kucultmek istedim.
net_listem$hp_oran <- net_listem$hp / 100

# Sonucu yazdirarak gormek istedim
print(net_listem)

# Mevcut sutun isimlerini kontrol etmek istedim
colnames(net_listem)

# Ilk 3 sutunun ismini BUYUK HARF yapmak istedim
colnames(net_listem)[1:3] <- c("YAKIT_VERIMI", "SILINDIR", "BEYGIR_GUCU")

# Kontrol etmek istedim
head(net_listem)

# Listemdeki araclarin Ortalama (Mean) yakit verimi degerlerini gormek istedim.
mean(net_listem$YAKIT_VERIMI)

# Listemdeki araclarin beygir gucune gore ceyrek degerlerini gormek istedim.
quantile(net_listem$BEYGIR_GUCU)

# Listemdeki araclarin median degerini gormek istedim. 
median(net_listem$BEYGIR_GUCU)

# Gorsellestirme yapmak istiyorum, daha onceki derste R'ye yukledigim paketi aktiflestirmem gerekir.
library(ggplot2)
ggplot(net_listem, aes(x = BEYGIR_GUCU, y = YAKIT_VERIMI)) + 
  geom_point(aes(color = SILINDIR)) + # Silindir hacimlerine gore renklendirdim
  facet_wrap(~ SILINDIR)  

# farkli sekilde de gorsellestirebiliriz
boxplot(net_listem$BEYGIR_GUCU, 
        main = "Beygir Gucu Dagilimi", 
        ylab = "Beygir Gucu (HP)", 
        col = "orange")

# Elde ettigim net listenin ozet bilgilerini gormek istedim. 
summary(net_listem)
View(net_listem)

# Kesifsel Veri Analizi
mean(net_listem$YAKIT_VERIMI) # yakit tuketim ortalamasi 12.62
quantile(net_listem$BEYGIR_GUCU) # IQR 215 ile 245 arasindadir.
median(net_listem$BEYGIR_GUCU)   # Sonuc: 230, 230 ile 245 beygir arasinda cok fazla benzer arac varken, 205 ile 230 arasinda araclar daha seyrek dagilmistir.