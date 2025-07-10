# Legatum Prosperity Index 2023: Classification of Countries via Clustering Analysis

This project aims to group 167 countries based on their prosperity profiles using data from the Legatum Prosperity Index 2023. The analysis was conducted in the R programming language, employing clustering algorithms to segment nations based on 12 fundamental pillars of prosperity, including economic quality, living conditions, health, education, and governance.


Bu proje, Legatum Prosperity Index 2023 verilerini kullanarak 167 ülkenin refah profillerine göre gruplandırılmasını amaçlamaktadır. Analizde, ülkeleri ekonomik kalite, yaşam koşulları, sağlık, eğitim ve yönetim gibi 12 temel refah göstergesi üzerinden kümeleme algoritmalarıyla segmentlere ayırmak için R programlama dili kullanılmıştır.

**Projenin temel hedefi:** Benzer refah yapılarına sahip ülkeleri belirleyerek küresel refah dağılımındaki temel örüntüleri ortaya çıkarmak ve ülkelerin güçlü/zayıf yönlerini karşılaştırmalı olarak analiz etmektir.

## Veri Seti

Analizde kullanılan veriler, Legatum Enstitüsü'nün resmi web sitesinden elde edilmiştir.

Kaynak: Legatum Prosperity Index 2023 [https://index.prosperity.com/rankings]

Kapsam: 167 ülke, 12 refah boyutu (pillar)

Veri setindeki 12 ana boyut şunlardır:

Safety & Security: Güvenlik ve Emniyet

Personal Freedom: Bireysel Özgürlükler

Governance: Yönetim Kalitesi

Social Capital: Sosyal Sermaye

Investment Environment: Yatırım Ortamı

Enterprise Conditions: Girişimcilik Koşulları

Infrastructure & Market Access: Altyapı ve Pazar Erişimi

Economic Quality: Ekonomik Kalite

Living Conditions: Yaşam Koşulları

Health: Sağlık

Education: Eğitim

Natural Environment: Doğal Çevre

## Analiz Süreci ve Metodoloji

Proje, aşağıdaki adımları izleyerek gerçekleştirilmiştir:

### Veri Ön İşleme: 

Değişkenler arasındaki ölçek farklılıklarını gidermek için veriler Z-skor normalizasyonu (scale()) ile standartlaştırılmıştır.

### Korelasyon Analizi: 

Değişkenler arasındaki yüksek doğrusal ilişkiler (örn. Yaşam Koşulları ve Eğitim arasında r≈0,94) tespit edilmiş, bu durum boyut indirgeme ihtiyacını ortaya koymuştur.

### Boyut İndirgeme (Temel Bileşen Analizi - PCA):

* Yüksek korelasyonlu 12 değişkenin içerdiği bilgiyi daha az sayıda ve birbirinden bağımsız değişkene indirgemek amacıyla PCA uygulanmıştır.

* Toplam varyansın %83.4'ünü açıklayan ilk iki temel bileşen analiz için yeterli bulunmuştur.

* PC1: Genel Refah (Overall Prosperity): Varyansın %74.5'ini açıklayan bu bileşen, tüm refah göstergelerinin ortak eğilimini temsil eder.

* PC2: Sosyo-ekonomik Gelişmişlik ↔ Özgürlük-Doğa: Varyansın %8.9'unu açıklayan bu bileşen, ülkeleri sosyo-ekonomik göstergeler (Sağlık, Eğitim) ile Bireysel Özgürlükler ve Doğal Çevre arasındaki dengeye göre ayırmaktadır.

### Kümeleme Algoritmalarının Değerlendirilmesi: En uygun küme yapısını bulmak için birden fazla algoritma denenmiş ve karşılaştırılmıştır:

* K-Means
* K-Medoids (PAM)
* Hiyerarşik Kümeleme (Ward.D2, Complete, Average metotları ile)
* Model Tabanlı Kümeleme
* Yoğunluk Tabanlı Kümeleme (DBSCAN)

### Optimal Modelin Seçimi: 

Yapılan karşılaştırmalar sonucunda, istatistiksel geçerliliği en yüksek ve en iyi yorumlanabilir sonuçları üreten modelin PCA + 3-Kümeli K-Means olduğuna karar verilmiştir.

## Temel Bulgular ve Sonuçlar

Seçilen nihai model, ülkeleri refah profillerine göre üç anlamlı ve birbirinden net bir şekilde ayrışan kümeye ayırmıştır.

### Küme 2: Yüksek Refah ve Dengeli Yaşam Ülkeleri (49 Ülke)

Temsilciler: İskandinav ülkeleri (Danimarka, Norveç, İsveç), İsviçre, Almanya, Kanada, Japonya, Avustralya.

Profil: Tüm 12 refah göstergesinde en yüksek skorlara sahip, istikrarlı ve dengeli ülkelerdir. Güçlü ekonomilerinin yanı sıra yüksek bireysel özgürlükler, sağlam sosyal devlet yapıları ve kaliteli çevre sunarlar.

### Küme 3 : Orta ve Gelişmekte Olan Ülkeler (77 Ülke)

Temsilciler: Türkiye, Çin, Rusya, Brezilya, Meksika, Hindistan, Endonezya ve Suudi Arabistan gibi Körfez ülkeleri.

Profil: En kalabalık ve heterojen kümedir. Genellikle hızlı ekonomik büyüme ve sanayileşme yaşayan, ancak bu süreçte yönetim, bireysel özgürlükler ve çevresel konularda zorluklarla karşılaşan "yükselen güçleri" temsil eder. Türkiye de bu kümede yer almaktadır.

### Küme 1 : Düşük Refah ve Kırılgan Ülkeler (41 Ülke)

Temsilciler: Somali, Çad, Afganistan, Yemen, Suriye ve Sahra Altı Afrika'daki birçok ülke.

Profil: Yüksek yoksulluk, siyasi istikrarsızlık, zayıf yönetim ve temel hizmetlere sınırlı erişim gibi yapısal sorunlarla mücadele eden ülkelerdir. Tüm refah göstergelerinde en düşük puanlara sahiptirler.

### Öne Çıkan Gözlem:

Refah, Sadece Ekonomik Zenginlik Değildir

Bu analizin en çarpıcı bulgularından biri, refahın çok boyutlu yapısını ortaya koymasıdır. Örneğin:

Yüksek gelirli petrol ülkeleri (Suudi Arabistan, BAE, Katar) ekonomik göstergelerde güçlü olmalarına rağmen bireysel özgürlükler ve yönetim kalitesi gibi alanlarda daha düşük puanlar alarak Orta Refah (Mavi Küme) grubunda yer almıştır.

Yine yüksek gelirli olan İskandinav ülkeleri ise sadece ekonomide değil, sosyal, demokratik ve çevresel göstergelerde de dengeli ve çok yüksek bir performans sergileyerek Yüksek Refah (Yeşil Küme) grubunda ayrışmıştır.

Bu durum, PCA ve kümeleme analizinin, ülkeleri yüzeysel benzerliklerine göre değil, derinlemesine refah yapılarına göre ne kadar başarılı bir şekilde gruplandırdığını kanıtlamaktadır.

### Nihai Modelin Seçim Gerekçeleri (PCA + 3-Kümeli K-Means)

Bu modelin seçilmesinin temel nedenleri şunlardır:

**İstatistiksel Geçerlilik:**
Silüet Skoru: 0.495 ile denenen tüm modeller arasında en yüksek ortalama silüet skorunu vermiştir. Bu, kümelerin kendi içinde tutarlı ve birbirinden iyi ayrıştığını gösterir.

Açıklanan Varyans: Kümeler arası varyansın toplam varyansa oranı %80.1'dir. Bu, modelin veri setindeki farklılıkları başarıyla yakaladığını gösteren çok güçlü bir metriktir.

**Yorumlanabilirlik ve Görsel Ayrışım:**

PCA uzayında kümeler (Yeşil, Mavi, Kırmızı) görsel olarak birbirinden net bir şekilde ayrılmıştır.

Oluşturulan üç küme (Yüksek, Orta, Düşük Refah) kolayca yorumlanabilen, mantıksal ve tutarlı profiller sunmaktadır.

Dengeli Küme Boyutları: Model, veri setini doğal yapılarına uygun şekilde 41, 49 ve 77 gibi dengeli üye sayılarına sahip kümelere ayırmıştır.

## Repo Yapısı

```text
.
├── Kapsamli_Rapor.pdf       # Analizin tüm detaylarını içeren orijinal rapor
├── data/
│   └── prosperity_index_2023.csv  # Analizde kullanılan ham veri
├── scripts/
│   └── clustering_analysis.R    # Veri analizi ve görselleştirme için kullanılan R betiği
└── README.md                    # Bu dosya
```

## Kullanılan Teknolojiler

### Programlama Dili: R

### Temel Paketler: tidyverse, cluster, factoextra, dendextend, NbClust
