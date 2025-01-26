setwd("C:/Users/lyasm/Documents/RStudio/Dossier")
# on installe la bibliothèque pour travailler avec .xlsx :
install.packages("readxl")  
library(readxl)

# on importe les données :
data1 = read_excel("test_formants_phrases.xlsx")
data2 = read_excel("test_formants_presentation.xlsx")

## LE TRAITEMENT ET LE NETTOYAGE DES DONNEES :
# on suprimme la colonne inutile dans data1 :
data1$...1 = NULL

# on vérifie les types de données :
str(data1)
str(data2)

# on examine s'il y a des occurrences vides :
sum(is.na(data1))
sum(is.na(data2))

# on regarde lesquelles sont vides :
which(is.na(data2))
data2[!complete.cases(data2), ]

# il n'y en a que trois, on peut les supprimer. on crée un nouveau tableau à la base de data2 :
data2_clean = data2[complete.cases(data2), ]

colnames(data1)[colnames(data1) == "DURÉE"] = "DUREE"
colnames(data2_clean)[colnames(data2_clean) == "DURÉE"] = "DUREE"

summary(data1$DUREE)
summary(data2_clean$DUREE)

# data2_clean comprend plus de données que data1. il est nécessaire d'équilibrer le nombre de données pour que l'analyse soit pertinente.
# on définie combien de lignes on doit avoir au data2 :
needed_rows = nrow(data1)  

# on supprime les lignes supplémentaires au hasard :
set.seed(42)
data2_balanced = data2[sample(nrow(data2), needed_rows), ] 

# on vérifie le nombre de lignes (on nécessite 163)
nrow(data2_balanced) 


## LA VISUALISATION :
# on observe les valeurs aberrantes dans deux tableaux :
ggplot(data1, aes(x = APERTURE, y = DUREE)) +
  geom_boxplot() +
  labs(title = "Boxplot de la durée pour data1", x = "Aperture", y = "Durée")

ggplot(data2_balanced, aes(x = APERTURE, y = DUREE)) +
  geom_boxplot() +
  labs(title = "Boxplot de la durée pour data2", x = "Aperture", y = "Durée")


# on crée un histogramme "la distribution de la durée" pour data1:
ggplot(data1, aes(x = DUREE)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution de la durée (débit lent)", 
       x = "Durée (en secondes)", y = "Fréquence")

# et pour data2:
ggplot(data2_balanced, aes(x = DUREE)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution de la durée (débit rapide)", 
       x = "Durée (en secondes)", y = "Fréquence")


# on transforme la colonne APERTURE en facteur pour faire un histogramme :
combined_data$APERTURE = c(data1$APERTURE, data2_balanced$APERTURE)
levels(combined_data$APERTURE)
str(combined_data$APERTURE)
combined_data$APERTURE = as.factor(combined_data$APERTURE)

# on fait un histogramme pour voir la distribution des niveaux d'aperture :
ggplot(combined_data, aes(x = APERTURE)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution des niveaux d'aperture (données combinées)", 
       x = "Aperture", y = "Fréquence")

# on fait un histogramme pour voir la distribution de la durée selon l'aperture :
ggplot(combined_data, aes(x = DUREE, fill = APERTURE)) +
  geom_histogram(binwidth = 0.02, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Distribution de la durée selon l'aperture (données combinées)", 
       x = "Durée", y = "Fréquence") +
  theme_minimal()

# on fait un histogramme pour voir la distribution de la durée selon le débit :
ggplot(combined_data, aes(x = DUREE, fill = DEBIT)) +
  geom_histogram(binwidth = 0.01, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Distribution de la durée selon le débit (données combinées)', 
       x = 'Durée', y = 'Fréquence') +
  scale_fill_manual(values = c('blue', 'red'), 
                    labels = c('Débit lent', 'Débit rapide'))

# on fait un scatterplot pour aperture + débit :
ggplot(combined_data, aes(x = APERTURE, y = DUREE, color = DEBIT)) +
  geom_point() +
  labs(title = "Durée en fonction de l'aperture et du débit 
       (données combinées)", 
       x = "Aperture", y = "Durée (en secondes)") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()


# on fait l'ANOVA à deux facteurs :
aov_result = aov(DUREE ~ APERTURE * DEBIT, data = combined_data)
summary(aov_result)

# on fait le test-t :
t_test_debit = t.test(DUREE ~ DEBIT, data = combined_data)

