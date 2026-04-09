# Veri Madenciliginde Kullanilan Temel Fonksiyonlar

# Veri madenciliginde 'tidyverse' genel bir kutuphane olarak 
# (dplyr, ggplot2, tidyr) olmazsa olmazdir.

# 1. Gerekli paketleri yuklemek gerekiyor.
install.packages("tidyverse")
install.packages("skimr")
library(tidyverse)
library(skimr)   

# 2. Veri Setini hazirlamak gerekiyor.
# Ornek olarak R icinde hazir bulunan 'iris'i kullandim, df ile cagirdim.
df <- as_tibble(iris)

# 3. Veriyi kesfetmem gerekiyor.
glimpse(df)       # Veri tipleri ve ilk degerler
summary(df)       # Temel istatistiksel ozet (Min, Max, Mean...)
skim(df)          # Daha detayli ve gorsel bir ozet (Eksik veri ve dagilim dahil)

# 3.1. Eksik Veri Kontrolu
# Veri madenciliginde eksik veri analizi bozabilir.
anyNA(df)                       # Hic eksik veri var mi? (TRUE/FALSE)
colSums(is.na(df))              # Sutun sutun eksik veri sayisi

# 3.2. Dagilim ve Aykiri Deger (Outlier) Analizi
# Sayisal bir degiskenin dagilimini gormek icin:
ggplot(df, aes(x = Sepal.Length)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Sepal Length Dagilimi")

# Aykiri degerleri gormek icin Boxplot:
ggplot(df, aes(y = Sepal.Length, x = Species)) +
  geom_boxplot()


# 4. VERI ON ISLEME
# Amacimiz: Veriyi analiz edilebilir ve temiz bir hale getirmek.

# 4.1. Sutun Adlarini Duzenleme 
# Tum sutun isimlerini kucuk harfe cevirmek standart bir yaklasimdir.
df_clean <- df %>% 
  rename_all(tolower)

# 4.2. Filtreleme ve Secme
df_subset <- df_clean %>%
  filter(species != "setosa") %>%
  select(sepal.length, sepal.width, species)

# 4.3. Yeni Degisken Turetme 
df_preprocessed <- df_subset %>%
  mutate(sepal_area = sepal.length * sepal.width, # Alan hesaplama
         size_cat = if_else(sepal.length > 5.5, "Large", "Small")) # Kategorize etme

# 4.4. Normalizasyon / Standartlastirma 
df_scaled <- df_preprocessed %>%
  mutate(across(where(is.numeric), scale)) # Sayisal sutunlari standartlastirir (mean=0, sd=1)

# 5. SONUCLARIN OZETLENMESI
# Gruplandirarak istatistik alma:
summary_table <- df_preprocessed %>%
  group_by(species, size_cat) %>%
  summarise(
    avg_length = mean(sepal.length),
    count = n(),
    .groups = 'drop'
  )

print(summary_table)



# Veri madenciliginde eksik veri tespiti ve ortalama ile doldurma temel bir fonksiyondur.
# Sentetik bir veri seti uzerinde eksik veri olusturup doldurma islemi:

set.seed(123) # Sonuclarin her seferinde ayni cikmasi icin 'seed' belirliyoruz
df_with_nas <- df_clean %>%
  mutate(sepal.length = ifelse(row_number() %in% sample(1:n(), 10), NA, sepal.length))

# 1. Kac tane bos veri olustugunu gormek istiyorum
print(paste("Eksik deger sayisi:", sum(is.na(df_with_nas$sepal.length))))

# 2. Eksik Verileri Ortalama (Mean) ile Dolduralım
# Veri madenciliginde buna 'Mean Imputation' denir.
df_filled <- df_with_nas %>%
  mutate(sepal.length = if_else(
    is.na(sepal.length),               
    mean(sepal.length, na.rm = TRUE),   
  ))

# Kontrol edelim
print(paste("Doldurma sonrasi eksik deger sayisi:", sum(is.na(df_filled$sepal.length))))
