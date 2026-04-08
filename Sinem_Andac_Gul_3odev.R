# Veri Uretilerek Kesifsel Veri Analizi
# Gerekli Paketlerin Yuklenmesi
install.packages("tidyverse") # Veri manipulasyonu ve ggplot2 gorsellestirmeleri icin kullandigimiz paketi yukledik.
install.packages("caret")     # Korelasyon filtreleme ve model hazirlik surecleri icin kullandigimiz paketi yukledik.
install.packages("corrplot")   # Korelasyon matrislerini gorsellestirmek icin kullandigimiz paketi yukledik.
install.packages("zoo")        # Eksik verileri doldurma (na.locf vb.) islemleri icin kullandigimiz paketi yukledik.     
install.packages("caTools")    # Veriyi egitim ve test olarak bolmek icin kullandigimiz paketi yukledik.  
install.packages("readxl")     # Excel dosyalarindan veri okumak icin kullandigimiz paketi yukledik.


library(caret)#Yukledigimiz paketleri R'ye hatirlattik.
library(corrplot)
library(zoo)
library(caTools)
library(readxl)
library(ggplot2)

# Sentetik veri uretmek icin rastgele ve tekrarlanabilir, toplamda 100'den az olacak sekilde veri urettim 
set.seed(123)
n <- 100

# Bagimsiz degiskenlerin normal dagilima (rnorm) uygun olarak uretilmesi
x1 <- rnorm(n, 50, 10)  # Ortalama=50, SS=10 olan bagimsiz degisken 1 - Yas
x2 <- rnorm(n, 100, 20) # Ortalama=100, SS=20 olan bagimsiz degisken 2 - Gelir

# Bagimli degiskenin (y) olusturulmasi
y <- 5 + 1.2*x1 + 0.7*x2 + rnorm(n, 0, 5)

# Degiskenlerin bir veri cercevesinde (Data Frame) birlestirilmesi
data_i <- data.frame(y, x1, x2)

# View fonksiyonu ile olusturulan veri setinin tablo formatinda incelenmesi
# View(data_i)

# Eksik veri yonetimi
data_i$x1[sample(1:n, 5)] <- NA
ortalama_x1 <- mean(data_i$x1, na.rm = TRUE)
data_i$x1 <- ifelse(is.na(data_i$x1), ortalama_x1, data_i$x1)

# Aykiri deger (Outlier) filtreleme
Q1 <- quantile(data_i$y, 0.25)
Q3 <- quantile(data_i$y, 0.75)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# Filtreleme islemini yapalim
library(dplyr)
data_temiz <- data_i %>%
  filter(y >= lower_bound & y <= upper_bound)

# Sonucu kontrol edelim
head(data_temiz) # Tabloda hic NA gorunmuyor

# Temizlenmis ortalamaya gore etiketleme
data_temiz <- data_temiz %>%
  mutate(Seviye = ifelse(y > mean(y), "Yuksek", "Dusuk"))

data_temiz$Seviye <- as.factor(data_temiz$Seviye)

# Veri Ozetleme 
# Veri setindeki tum degiskenlerin min, max, medyan ve ceyreklik degerlerinin incelenmesi
summary(data_temiz)
# Sayisal degerlerin standart sapma ve varyansini gosterir.
sd(data_temiz$y); var(data_temiz$y)
# Kategorik frekans tablosu
table(data_temiz$Seviye)

# Korelasyon Isi Haritasi
corr_mat <- cor(data_temiz[, 1:3])
corrplot(corr_mat, method = "color", addCoef.col = "black")

# Aykiri degerlerin gorsellestirilmesi
boxplot(data_temiz, 
        main = "Boxplot ile Aykiri Deger Analizi",
        ylab = "Degerler",
        col = "lightblue",
        border = "darkblue",
        pch = 19,      
        col.points = "red") 

# Gruplar Arasi Fark Gorseli (Boxplot)
ggplot(data_temiz, aes(x=Seviye, y=y, fill=Seviye)) +
  geom_boxplot() +
  labs(title="Gelir Seviyesine Gore Harcama Farklari", y="Harcama Puani")


# H0: Gelir seviyesi (Dusuk/Yuksek) harcama miktari uzerinde anlamli bir fark yaratmaz.
# H1: Gelir seviyesi yuksek olan grubun harcama miktari, dusuk olan gruptan anlamli derecede farklidir.

# Gruplar arasi farkin anlamliligini test etmek icin Bagimsiz Orneklemler T-Testi uyguluyoruz:
t_test_sonuc <- t.test(y ~ Seviye, data = data_temiz)
summary(data_temiz)

# Sonuclarin Yazdirilmasi
print(t_test_sonuc)

# Bagimsiz Orneklemler T-Testi (Independent Samples T-Test)
t_test_sonuc <- t.test(y ~ Seviye, data = data_temiz, 
                       var.equal = TRUE, # Varyanslarin esit oldugunu varsayiyoruz
                       conf.level = 0.95) # %95 guven duzeyi

# Sonuclari ekrana getir
print(t_test_sonuc)

# Hipotez Yorumu
# Yapilan bagimsiz orneklemler t-testi sonucuna gore, yuksek gelir grubunun harcama ortalamasi 149.84 iken
# dusuk gelir grubunun harcama ortalamasi 121.65 olarak saptanmistir. 
# Iki grup arasindaki bu fark istatistiksel olarak anlamli bulunmustur (t-degeri: -13.93, p-degeri: 0.001'den kucuk). 
# Bu bulgu, bireylerin gelir duzeylerinin harcama miktarlari uzerinde guclu ve belirleyici bir etkisi oldugunu kanitlamaktadir.

# --- URL UZERINDEN VERI SETI ILE KESIFSEL ANALIZ ---

# Diyabet Hastaligi Belirtileri Uzerine Kesifsel Analiz
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv"
basliklar <- c("Hamilelik", "Glikoz", "KanBasinci", "DeriKalinligi", "Insulin", "BMI", "SoyGecis", "Yas", "Sonuc")
data_ii <- read.csv(url, header = FALSE, col.names = basliklar)

# Veriyi daha duzenli olan 'tibble' formatina donusturuyoruz 
data_ii <- as_tibble(data_ii)

# Biyolojik olarak imkansiz '0' degerlerini tespit etme ve NA atama
data_ii$Glikoz[data_ii$Glikoz == 0] <- NA
data_ii$KanBasinci[data_ii$KanBasinci == 0] <- NA
data_ii$BMI[data_ii$BMI == 0] <- NA
data_ii$Insulin[data_ii$Insulin == 0] <- NA
data_ii$DeriKalinligi[data_ii$DeriKalinligi == 0] <- NA

# Aykiri degerleri simdi temizliyoruz
Q1 <- quantile(data_ii$KanBasinci, 0.25, na.rm = TRUE)
Q3 <- quantile(data_ii$KanBasinci, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
ust_sinir <- Q3 + 1.5 * IQR_val
alt_sinir <- Q1 - 1.5 * IQR_val

# Veriyi bu sinirlara gore filtreliyoruz
data_ii <- data_ii %>% 
  filter((KanBasinci <= ust_sinir & KanBasinci >= alt_sinir) | is.na(KanBasinci))

# Eksik verileri ortalama ile doldurma 
mean_glikoz <- mean(data_ii$Glikoz, na.rm = TRUE)
data_ii$Glikoz <- ifelse(is.na(data_ii$Glikoz), mean_glikoz, data_ii$Glikoz)

mean_kan <- mean(data_ii$KanBasinci, na.rm = TRUE)
data_ii$KanBasinci <- ifelse(is.na(data_ii$KanBasinci), mean_kan, data_ii$KanBasinci)

mean_bmi <- mean(data_ii$BMI, na.rm = TRUE)
data_ii$BMI <- ifelse(is.na(data_ii$BMI), mean_bmi, data_ii$BMI)

# Insulin ve Deri Kalinligi icin medyan kullandim (aykiri degerler oldugu icin)
median_ins <- median(data_ii$Insulin, na.rm = TRUE)
data_ii$Insulin <- ifelse(is.na(data_ii$Insulin), median_ins, data_ii$Insulin)

median_deri <- median(data_ii$DeriKalinligi, na.rm = TRUE)
data_ii$DeriKalinligi <- ifelse(is.na(data_ii$DeriKalinligi), median_deri, data_ii$DeriKalinligi)

# Temizlik ve doldurma sonrasi genel ozet 
summary(data_ii)

# Kategorik veri (Sonuc: 0-Saglikli, 1-Diyabet) icin frekans tablosu
table(data_ii$Sonuc) # Saglikli 500, diyabetli 268 kisi var

# VERI GORSELLESTIRME
# BMI (Vucut Kitle Indeksi) dagilimi (Histogram)
hist(data_ii$BMI, col="skyblue", main="BMI Dagilimi", xlab="BMI Degeri")

# Glikoz ve Yas iliskisi (Scatter Plot)
ggplot(data_ii, aes(x=Yas, y=Glikoz)) + 
  geom_point(aes(color=as.factor(Sonuc))) + 
  labs(title="Yas ve Glikoz Seviyesi Iliskisi (Renklendirme: Sonuc)")

# Facet Wrap ile ayristirma
ggplot(data_ii, aes(x=BMI, y=Glikoz)) +
  geom_point(alpha=0.5) +
  facet_wrap(~Sonuc) +
  labs(title="Diyabet Durumuna Gore BMI ve Glikoz Iliskisi")

# Aykiri degerlerin gorsellestirilmesi
boxplot(data_ii, 
        main = "Boxplot ile Aykiri Deger Analizi",
        ylab = "Degerler",
        col = "lightblue",
        border = "darkblue",
        pch = 19)

# KORELASYON VE ILISKI ANALIZI
korelasyon_matrisi <- cor(data_ii[, 1:8]) # Sadece sayisal sutunlar
corrplot(korelasyon_matrisi, method = "color", addCoef.col = "black")

# Model ve Hipotez
model_test <- lm(Sonuc ~ Glikoz + BMI + Yas, data = data_ii)
summary(model_test)

# HIPOTEZ YORUMU
# Degiskenler diyabet sonucundaki degisimin yaklasik %29.5'ini (Adjusted R-squared: 0.292) aciklamaktadir.
# Bu sebeple daha yuksek Glikoz seviyesi, daha yuksek BMI ve ilerleyen yas, 
# diyabet riskini istatistiksel olarak anlamli ve pozitif yonde artirmaktadir deriz.