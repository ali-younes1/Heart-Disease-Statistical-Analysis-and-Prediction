#Importation du dataset

df = read.csv("D:/Downloads/HeartDisease (2).csv",sep=',')



#PARTIE 1 :        

#Préparation du données        

#Supression de la ligne numéro 16 car les valeurs ne sont pas bien orodnnées       

df = df[df$ECGRepos != "150",]


#On a aussi remarqué que les types des variables dans ce dataset ne sont pas       

#conformes avec les types fournis dans le document pdf       

#--> On va les corrigé :       

#1 : On va rendre les variables nominales -> numériques selon le document pdf        


df$GlycemieJeune <- as.integer(df$GlycemieJeune)
df$FrequenceCardiaqueMax <- as.integer(df$FrequenceCardiaqueMax)
df$DepressionAncienne <- as.double(df$DepressionAncienne)



#2 : On va rendre les variables numériques -> nominales selon le document pdf        


df$TensionArterielleRepos <- factor(df$TensionArterielleRepos)


#Calcul de taux des variables manquantes dans les données        


taux = sum(is.na(df)) / dim(df)[1]
cat('Taux des valeurs manquantes dans le dataset :',round(taux,3)*100,'%')


#4.3% << 5% on peux faire la suppression MAIS on est dans le cas MAR         

#c'est à dire la probabilité d’absence dépend de variables observées       

#donc on va les imputer.


#Avant l'imputation , on doit visualiser et identifier les données aberrantes pour choisir         

#la bonne fonction d'imputation (-> Mediane/Knn s'il des valeurs aberrantes extremes)        


#Visualisation des valeurs aberrantes :        



#Boxplot de la variable Age :
boxplot(df$Age)


#--> il existe quelques valeurs aberrantes       


#Boxplot de la variable GlycemieJeune :        


boxplot(df$GlycemieJeune)


#--> Il existe quelques valeurs aberrantes       


#Boxplot de la variable Cholesterol :        


boxplot(df$Cholesterol)


#--> Il existe plusieurs valeurs aberrantes        


#Boxplot de la variable MaladieCardiaque :       


boxplot(df$MaladieCardiaque)


#--> Il n'existe aucune valeur aberrante ici       


#Boxplot de la variable Frequence Cardiaque Max        

boxplot(df$FrequenceCardiaqueMax)


#--> il existe quelque variables aberrantes.       


#Boxplot de la variable Depression Ancienne        


boxplot(df$DepressionAncienne)


#--> il existe quelques variables aberrantes.        


#Puisqu'il y a plusieurs valeurs aberrantes , on va appliquer l'imputation en utilisant la mediane ou l'algorithme Knn mais pas la moyenne car elle est sensible aux valeurs aberrantes.        


#Visualisation des valeurs manquantes :        


install.packages(naniar)
library(naniar)
gg_miss_var(df)



#Imputation des valeurs aberrantes       


install.packages(Hmisc)
library(Hmisc)



#Méthode 1 : Imputation par la médiane :       


#1 : Imputation des valeurs manquantes associés à la variable Age :        



df[is.na(df[['Age']]), 'Age'] = median(df[['Age']], na.rm = TRUE)
cat("On a ",sum(is.na(df$Age))," valeurs manquantes dans la variable Age")



#2 : Imputation des valeurs manquantes associés à la variable Cholesterol :        




df[is.na(df[['Cholesterol']]), 'Cholesterol'] = median(df[['Cholesterol']], na.rm = TRUE)
cat("On a ",sum(is.na(df$Cholesterol))," valeurs manquantes dans la variable Cholesterol")


#3 : Imputation des valeurs manquantes associés à la variable TensionArterielleRepos :       

#--> TensionArterielleRepos est une variable qualitative , donc on va appliquer l'imputation par mode.       


install.packages("modeest")
library(modeest)




#On calcule le mode :        


mode_val <- mfv(df$TensionArterielleRepos, na.rm = TRUE)



#Puis l'imputation :       


df$TensionArterielleRepos[is.na(df$TensionArterielleRepos)] <- mode_val
cat("Il y a", sum(is.na(df$TensionArterielleRepos)), "valeurs manquantes dans la variable TensionArterielleRepos.")



#4 : Imputation des valeurs manquantes associés à la variable GlycemieJeune :    



df[is.na(df[['GlycemieJeune']]), 'GlycemieJeune'] = median(df[['GlycemieJeune']], na.rm = TRUE)
cat("On a ",sum(is.na(df$GlycemieJeune))," valeurs manquantes dans la variable Glycémie jeune")


#5 : Imputation des valeurs manquantes associés à la variable MaladieCardiaque :       



df[is.na(df[['MaladieCardiaque']]), 'MaladieCardiaque'] = median(df[['MaladieCardiaque']], na.rm = TRUE)
cat("On a ",sum(is.na(df$MaladieCardiaque))," valeurs manquantes dans la variable MaladieCardiaque")



#6 : Imputation des valeurs manquantes associés à la variable DepressionAncienne :       


df[is.na(df[['DepressionAncienne']]), 'DepressionAncienne'] = median(df[['DepressionAncienne']], na.rm = TRUE)
cat("On a ",sum(is.na(df$DepressionAncienne))," valeurs manquantes dans la variable DepressionAncienne")




#6 : Imputation des valeurs manquantes associés à la variable FrequenceCardiaqueMax :        



df[is.na(df[['FrequenceCardiaqueMax']]), 'FrequenceCardiaqueMax'] = median(df[['FrequenceCardiaqueMax']], na.rm = TRUE)
cat("On a ",sum(is.na(df$FrequenceCardiaqueMax))," valeurs manquantes dans la variable FrequenceCardiaqueMax")



#Pour s'assurer :        


gg_miss_var(df)


#--> Selon le graphe , on a aucune valeur manquante.       


#On a aussi detecter qu'ils existent des chaines vides dans le dataset       



chaines_vides <- sapply(df, function(col) {
  if (is.factor(col) || is.character(col)) {
    sum(col == "") / length(col) * 100
  } else {
    NA  
  }
})
print(chaines_vides)



#-->On remarque que les pourcentages des chaines vides dans les variables suivantes : 
#[Sexe,TypeDouleurThoracique,ECGRepos, PenteSTExercice,AngineExercice] sont tres inférieures à 5% , donc on peux les supprimer.        


#Supression des chaines vides :        


liste = c('Sexe','TypeDouleurThoracique','ECGRepos', 'PenteSTExercice','AngineExercice')

for (col in liste) {
  df[[col]][df[[col]] == ""] <- NA
  df[[col]] <- as.factor(df[[col]])  # Convert to factor
  df <- df[!is.na(df[[col]]), ]  # Remove rows with NA in this column
}



#--> Pour s'assurer :        


chaines_vides <- sapply(df, function(col) {
  if (is.factor(col) || is.character(col)) {
    sum(col == "") / length(col) * 100
  } else {
    NA  
  }
})
chaines_vides



#Traitement des valeurs aberrantes       

#Ne pas traiter les variables binaires (0,1)       



c=names(df)

list=c("Age","Cholesterol","FrequenceCardiaqueMax","DepressionAncienne")


for (col in names(df)[list]) {
  Q1 <- quantile(df[[col]], 0.25)
  Q3 <- quantile(df[[col]], 0.75)
  IQR_value <- Q3 - Q1
  
  min <- Q1 - 1.5 * IQR_value
  max <- Q3 + 1.5 * IQR_value
  
  # Identifier et remplacer les  outliers avec la mediane
  outliers <- df[[col]] < min | df[[col]] > max
  df[[col]][outliers] <- median(df[[col]], na.rm = TRUE)
}



#Cholesterol       



for (i in 1:11){
  Q3_Cholesterol = quantile(df$Cholesterol, 0.75)
  Q1_Cholesterol = quantile(df$Cholesterol, 0.25)
  IQR_Cholesterol = Q3_Cholesterol - Q1_Cholesterol
  min_Cholesterol = Q1_Cholesterol - 1.5 * IQR_Cholesterol
  max_Cholesterol = Q3_Cholesterol + 1.5 * IQR_Cholesterol
  
  outliers_Cholesterol = which(df$Cholesterol < min_Cholesterol | df$Cholesterol > max_Cholesterol)
  df$Cholesterol[outliers_Cholesterol] = impute(df$Cholesterol, fun = median)
}




#Standarisation du dataset       

#Appliquez la normalisation Min-Max aux colonnes numériques        


df[, list] <- scale(df[, list], center = FALSE, scale = apply(df[, list], 2, max) - apply(df[, list], 2, min))
summary(df)





#Analyse univariée:        

#Variables Qualitatives:       


install.packages(ggplot2)
library(ggplot2)



#SEXE:       


table(df$Sexe)



ggplot(df, aes(x = Sexe, fill = Sexe)) +
  geom_bar() +
  labs(title = "Bar Plot of Sexe Variable", x = "Sexe", y = "Count") +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink"))



#TypeDouleurThoracique:        


table(df$TypeDouleurThoracique)




ggplot(df, aes(x =TypeDouleurThoracique, fill = TypeDouleurThoracique)) +
  geom_bar() +
  labs(title = "Bar Plot of TypeDouleurThoracique Variable", x = "TypeDouleurThoracique", y = "Count") +
  scale_fill_manual(values = c("ASY" = "blue", "ATA" = "black","NAP"="red","TA"="green"))



#TensionArterielleRepos:       


table(df$TensionArterielleRepos)



ggplot(df, aes(x =TensionArterielleRepos, fill = TensionArterielleRepos)) +
  geom_bar() +
  labs(title = "Bar Plot of TensionArterielleRepos", x = "TensionArterielleRepos", y = "Count")


#ECGRepos        


table(df$ECGRepos)




ggplot(df, aes(x =ECGRepos, fill = ECGRepos)) +
  geom_bar() +
  labs(title = "Bar Plot of ECGRepos Variable", x = "ECGRepos", y = "Count") +
  scale_fill_manual(values = c("LVH" = "blue", "Normal" = "green","ST"="red"))



#AngineExercice:       


table(df$AngineExercice)



ggplot(df, aes(x =AngineExercice, fill = AngineExercice)) +
  geom_bar() +
  labs(title = "Bar Plot of AngineExercice Variable", x = "AngineExercice", y = "Count") +
  scale_fill_manual(values = c("N" = "red", "Y" = "green"))




#PenteSTExercice:        


table(df$PenteSTExercice)



ggplot(df, aes(x =PenteSTExercice, fill = PenteSTExercice)) +
  geom_bar() +
  labs(title = "Bar Plot of PenteSTExercice Variable", x = "PenteSTExercice", y = "Count") +
  scale_fill_manual(values = c("Down" = "red", "Flat" = "green","Up"="blue"))



#MaladieCardiaque:       


table(df$MaladieCardiaque)



ggplot(df, aes(x =MaladieCardiaque, fill = MaladieCardiaque)) +
  geom_bar() +
  labs(title = "Bar Plot of MaladieCardiaque Variable", x = "MaladieCardiaque", y = "Count") +
  scale_fill_manual(values = c("0" = "red", "1"="blue"))



#Variables Quantitatives:        

#Age:        


summary(df$Age)
shapiro.test(df$Age)
hist(df$Age, main = "Distribution de l'âge", xlab = "Âge", col = "skyblue")


#Cholesterol:        


summary(df$Cholesterol)
shapiro.test(df$Cholesterol)
hist(df$Cholesterol, main = "Distribution du cholestérol", xlab = "Cholestérol", col = "lightgreen")


#GlycemieJeune:        


summary(df$GlycemieJeune)
shapiro.test(df$GlycemieJeune)
hist(df$GlycemieJeune, main = "Distribution de la GlycemieJeune", xlab = 'GlycemieJeune', col = "red")


#Analyse Bivariée :        

#On a 2 cas :        

 #==>1er cas  : Quanti-Quanti : (la variable cible quantitative est : Cholesterol) (On va effectuer donc des tests de spearman(car les distributions ne suivent pas une loi normale) et des scatterplots)        
 
#Cholesterol & Age :       
cor.test(df$Cholesterol,df$Age,method="spearman")
ggplot(df,aes(Age,Cholesterol))+geom_point()


 #les points sont trés dispérsés => indice de la non-corrélation.        
 
#==> Une légère corrélation positive (rho = 0,0652).       

  #La p-value est de 0,05086, juste au-dessus du seuil de signification classique de 0,05.
  #Cela suggère une tendance à la corrélation, mais elle n'est pas statistiquement significative à ce seuil. (presque nulle)       
    

#Cholesterol & GlycemieJeune :       
cor.test(df$Cholesterol,df$GlycemieJeune,method="spearman")
ggplot(df,aes(GlycemieJeune,Cholesterol))+geom_point()
#==>Statistique de test (S) : 119591123       

  #Valeur p : 0,8623       
    
  #La statistique de test (S) mesure la relation monotone entre les variables. Ici, la valeur p associée est de 0,8623.       
  
  #L'hypothèse alternative stipule que le vrai coefficient de corrélation (rho) entre Cholestérol et GlycémieJeune n'est pas égal à zéro. Les estimations sur l'échantillon suggèrent un coefficient de corrélation rho de 0,0058.      
  
  #Ces résultats indiquent une corrélation presque nulle entre Cholestérol et GlycémieJeune. Avec une valeur p élevée, bien au-dessus du seuil de signification classique de 0,05, il n'y a pas de corrélation significative entre ces variables dans l'échantillon observé.       
  

#Cholesterol & FrequenceCardiaqueMax :       
cor.test(df$Cholesterol,df$FrequenceCardiaqueMax,method="spearman")
ggplot(df,aes(FrequenceCardiaqueMax,Cholesterol))+geom_point()
#==>Statistique de test (S) : 117304850        

 # Valeur p : 0,4581       
  
  #La statistique de test (S) évalue la relation monotone entre les variables. La valeur p associée est de 0,4581.      
  
  #L'hypothèse alternative suggère que le vrai coefficient de corrélation (rho) entre Cholestérol et FréquenceCardiaqueMax n'est pas égal à zéro. Les estimations sur l'échantillon indiquent un coefficient de corrélation rho de 0,0248.        
  
  #Ces résultats indiquent une corrélation faible et non significative entre Cholestérol et FréquenceCardiaqueMax. Avec une valeur p supérieure au seuil de 0,05, cela suggère qu'il n'y a pas de corrélation significative entre ces variables dans l'échantillon observé.       
  


#Cholesterol & DepressionAncienne :
cor.test(df$Cholesterol,df$DepressionAncienne,method="spearman")
ggplot(df,aes(DepressionAncienne,Cholesterol))+geom_point()

#==> Statistique de test (S) : 113231611       

 # Valeur p : 0,07905        
  
  #La statistique de test (S) évalue la relation monotone entre les variables. La valeur p associée est de 0,07905.       
  
  #L'hypothèse alternative suggère que le vrai coefficient de corrélation (rho) entre Cholestérol et DépressionAncienne n'est pas égal à zéro. Les estimations sur l'échantillon indiquent un coefficient de corrélation rho de 0,0587.        
      
  #Ces résultats suggèrent une corrélation faible et borderline entre Cholestérol et DépressionAncienne. Avec une valeur p légèrement supérieure au seuil de 0,05, cela indique qu'il pourrait y avoir une tendance à la corrélation, mais elle n'est pas statistiquement significative dans l'échantillon observé.       
  

  #==> En résumé , les variables quantitatives ont une très faible (presque nulle) corrélation  avec la variable cible Cholesterol.        
  



#==> 2ème cas : Quanti-quali :(notre variable cible quantitative est Cholesterol)       


#------------------------------------------------------REMARQUE-----------------------------------------------------------------------       

      #     ==> On n'a pas la normalité des données  donc on est obligé de passer par un test non paramétrique.      


#Cholesterol & Sexe :
kruskal.test(Cholesterol ~ Sexe,data=df)

#==> Le test de Kruskal-Wallis indique une différence statistiquement significative dans les niveaux de cholestérol entre les groupes masculin et féminin.        

#La statistique du test est de 3.9494 avec 1 degré de liberté, et la valeur de p est égale à 0.04689, ce qui suggère une association statistiquement significative entre le sexe et les niveaux de cholestérol.        

#En résumé, il existe une relation significative entre le sexe et les niveaux de cholestérol dans nos données.       



#Cholesterol & TypeDouleurThoracique :
kruskal.test(Cholesterol ~ TypeDouleurThoracique,data=df)

#Le test de Kruskal-Wallis ne révèle pas de différence statistiquement significative dans les niveaux de cholestérol entre les divers types de douleur thoracique.       

#La statistique du test est de 4.2167 avec 3 degrés de liberté, et la valeur de p est égale à 0.239, indiquant l'absence d'une association statistiquement significative entre les différents types de douleur thoracique et les niveaux de cholestérol.       

#En résumé, le test ne supporte pas l'idée qu'il y a une relation entre ces variables dans nos données.        



#Cholesterol & TensionArterielleRepos :        

kruskal.test(Cholesterol ~ TensionArterielleRepos,data=df)
#Le test de Kruskal-Wallis montre une statistique du test de 86.383 avec 69 degrés de liberté.       

#Bien que la valeur de p soit légèrement supérieure au seuil de signification habituel de 0.05, avec une valeur de 0.07683, cela suggère une tendance vers une différence significative des niveaux de cholestérol en fonction de la tension artérielle au repos.        

#Cependant, cette différence n'atteint pas le seuil habituel de signification statistique.       

#En résumé, bien qu'il y ait une tendance, le test ne parvient pas à établir de manière significative une association entre la tension artérielle au repos et les niveaux de cholestérol dans nos données.        


#Cholesterol & ECGRepos :
kruskal.test(Cholesterol ~ ECGRepos,data=df)

#Le test de Kruskal-Wallis indique une statistique du test de 12.667 avec 2 degrés de liberté.       

#La valeur de p est de 0.001776, inférieure au seuil standard de 0.05, suggérant une différence statistiquement significative des niveaux de cholestérol en fonction des résultats de l'électrocardiogramme au repos.       

#En résumé, il y a une forte indication d'une association statistiquement significative entre ces variables dans nos données.        



#Cholesterol & AngineExercice :        

kruskal.test(Cholesterol ~ AngineExercice,data=df)

#Le test de Kruskal-Wallis révèle une statistique du test de 8.9295 avec 1 degré de liberté.       

#La valeur de p est de 0.002806, inférieure au seuil standard de 0.05.       

#Cela suggère une différence statistiquement significative des niveaux de cholestérol en fonction de la présence d'angine lors de l'exercice.        

#En résumé, il existe une forte indication d'une association statistiquement significative entre ces variables dans nos données.        



#Cholesterol & PenteSTExercice :       
kruskal.test(Cholesterol ~ PenteSTExercice,data=df)
#Le test de Kruskal-Wallis indique une statistique du test de 1.8352 avec 2 degrés de liberté.       

#La valeur de p est de 0.3995, supérieure au seuil standard de 0.05.       

#Cela suggère l'absence d'une différence statistiquement significative des niveaux de cholestérol en fonction de la pente du segment ST pendant l'exercice.       

#En résumé, le test n'a pas trouvé d'association statistiquement significative entre ces variables dans nos données.       



#--------------------------------------------------REGRESSION LINEAIRE  SIMPLE--------------------------------------       




 #Encodage des données:        
df$Sexe<- as.integer(df$Sexe)
df$TypeDouleurThoracique <- as.integer(df$TypeDouleurThoracique)
df$TensionArterielleRepos <- as.integer(df$TensionArterielleRepos)
df$ECGRepos <- as.integer(df$ECGRepos)
df$AngineExercice <- as.integer(df$AngineExercice)
df$PenteSTExercice <- as.integer(df$PenteSTExercice)


#Regression simple :       
#on effectue d'abord des tests de corrélation avec la variable cible avant de passer à la modélisation.        


#Cholesterol ~ Age :
ggplot(df,aes(df$Age,df$Cholesterol))+geom_point() 
#==> les points sont dipérsées ==> pas de relation linéaire .   
cor.test(df$Cholesterol,df$Age,method="spearman")

#==> coeff: 0.06 ==> tres faible.        

#Cholesterol ~ Sexe :        
ggplot(df,aes(df$Sexe,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$Sexe,method = 'spearman')

#==> coeff : -0.066 ==> tres faible.       


#Cholesterol ~ TypeDouleurThoracique :         

ggplot(df,aes(df$TypeDouleurThoracique,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$TypeDouleurThoracique,method = 'spearman')

#==> coeff : -0.05 ==> tres faible.        


#Cholesterol ~ TensionArterielleRepos :        
ggplot(df,aes(df$TensionArterielleRepos,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$TensionArterielleRepos,method = 'spearman')

#==> coeff : 0.077 ==> tres faible.        


#Cholesterol ~ GlycemieJeune       
ggplot(df,aes(df$GlycemieJeune,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$GlycemieJeune,method = 'spearman')

#==> coeff : 0.005 ==> trop faible.        


#Cholesterol ~  ECGRepos       
ggplot(df,aes(df$ECGRepos,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$ECGRepos,method = 'spearman')

#==> coeff : -0.064 ==> trop faible.       


#Cholesterol ~ FrequenceCardiaqueMax :       
ggplot(df,aes(df$FrequenceCardiaqueMax,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$FrequenceCardiaqueMax,method = 'spearman')

#==> coeff : 0.024 ==> trop faible.        


#Cholesterol ~ AngineExercice  :
ggplot(df,aes(df$AngineExercice,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$AngineExercice,method = 'spearman')

#Coeff : 0.099 plus élvées , mais encore faible.       

#Cholesterol ~ DepressionAncienne :        
ggplot(df,aes(df$DepressionAncienne,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$DepressionAncienne,method = 'spearman') 
#==> coeff : 0.058 ==> trop faible.        


#Cholesterol ~ PenteSTExercice  :        
ggplot(df,aes(df$PenteSTExercice,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$PenteSTExercice,method = 'spearman')

#==> coeff : -0.04 ==> trop faible.        


#Cholesterol ~ MaladieCardiaque :        
ggplot(df,aes(df$MaladieCardiaque,df$Cholesterol))+geom_point()
cor.test(df$Cholesterol,df$MaladieCardiaque,method = 'spearman')

#==> coeff : 0.074 ==> trop faible.        


#==> aucun modèle linéaire simple n'est possible vu que Cholesterol ne possède pas de corrélation  avec les autres variables.        



#------------------------------------------REGRESSION LINEAIRE MULTIPLE--------------------------------------------       

modele1 = lm(Cholesterol~.,data = df)
plot(modele1)

summary(modele1)


#==> Variables significatives : Les variables "Sexe", "AngineExercice" et "TensionArterielleRepos" présentent des p-values significatives (indiquées par les étoiles *), suggérant une relation statistiquement significative avec "Cholesterol".        

 # Variables non significatives : Les autres variables n'ont pas de lien significatif avec "Cholesterol", avec des p-values supérieures à 0.05.        
  

modele2 = lm(Cholesterol ~ Sexe + AngineExercice + TensionArterielleRepos,data=df)
plot(modele2)
summary(modele2)


#===> Ce modèle, bien qu'ayant un pouvoir de prédiction statistiquement significatif, explique toujours une faible proportion de la variance de "Cholesterol". Seules les variables "Sexe" et "AngineExercice" semblent avoir une influence significative sur "Cholesterol" dans ce modèle. La relation avec "TensionArterielleRepos" est moins claire mais montre une tendance à être significative.           

plot(df$Cholesterol, col = "blue", xlab = "Cholesterol", ylab = "Predictors")
abline(modele2, col = "red")


#TEST ANOVA :        

#Cholesterol & Sexe :       
kruskal.test(Cholesterol ~ Sexe,data=df)

#==> Le test de Kruskal-Wallis indique une différence statistiquement significative dans les niveaux de cholestérol entre les groupes masculin et féminin.        

#La statistique du test est de 3.9494 avec 1 degré de liberté, et la valeur de p est égale à 0.04689, ce qui suggère une association statistiquement significative entre le sexe et les niveaux de cholestérol.        

#En résumé, il existe une relation significative entre le sexe et les niveaux de cholestérol dans nos données.       



#Cholesterol & TypeDouleurThoracique :
kruskal.test(Cholesterol ~ TypeDouleurThoracique,data=df)

#Le test de Kruskal-Wallis ne révèle pas de différence statistiquement significative dans les niveaux de cholestérol entre les divers types de douleur thoracique.       

#La statistique du test est de 4.2167 avec 3 degrés de liberté, et la valeur de p est égale à 0.239, indiquant l'absence d'une association statistiquement significative entre les différents types de douleur thoracique et les niveaux de cholestérol.       

#En résumé, le test ne supporte pas l'idée qu'il y a une relation entre ces variables dans nos données.        



#Cholesterol & TensionArterielleRepos :        
kruskal.test(Cholesterol ~ TensionArterielleRepos,data=df)
#Le test de Kruskal-Wallis montre une statistique du test de 86.383 avec 69 degrés de liberté.       

#Bien que la valeur de p soit légèrement supérieure au seuil de signification habituel de 0.05, avec une valeur de 0.07683, cela suggère une tendance vers une différence significative des niveaux de cholestérol en fonction de la tension artérielle au repos.        

#Cependant, cette différence n'atteint pas le seuil habituel de signification statistique.       

#En résumé, bien qu'il y ait une tendance, le test ne parvient pas à établir de manière significative une association entre la tension artérielle au repos et les niveaux de cholestérol dans nos données.        


#Cholesterol & ECGRepos :        
kruskal.test(Cholesterol ~ ECGRepos,data=df)

#Le test de Kruskal-Wallis indique une statistique du test de 12.667 avec 2 degrés de liberté.       

#La valeur de p est de 0.001776, inférieure au seuil standard de 0.05, suggérant une différence statistiquement significative des niveaux de cholestérol en fonction des résultats de l'électrocardiogramme au repos.       

#En résumé, il y a une forte indication d'une association statistiquement significative entre ces variables dans nos données.        



#Cholesterol & AngineExercice :
kruskal.test(Cholesterol ~ AngineExercice,data=df)
#Le test de Kruskal-Wallis révèle une statistique du test de 8.9295 avec 1 degré de liberté.
#La valeur de p est de 0.002806, inférieure au seuil standard de 0.05.       

#Cela suggère une différence statistiquement significative des niveaux de cholestérol en fonction de la présence d'angine lors de l'exercice.        

#En résumé, il existe une forte indication d'une association statistiquement significative entre ces variables dans nos données.       



#Cholesterol & PenteSTExercice :    
kruskal.test(Cholesterol ~ PenteSTExercice,data=df)   

#Le test de Kruskal-Wallis indique une statistique du test de 1.8352 avec 2 degrés de liberté.
#La valeur de p est de 0.3995, supérieure au seuil standard de 0.05.        

#Cela suggère l'absence d'une différence statistiquement significative des niveaux de cholestérol en fonction de la pente du segment ST pendant l'exercice.       

#En résumé, le test n'a pas trouvé d'association statistiquement significative entre ces variables dans nos données.        





#REGRESSION LOGISTIQUE :       

cor.test(df$MaladieCardiaque,df$Age)
#==> La corrélation de Pearson entre "MaladieCardiaque" et "Âge" est significative (r = 0.275). Cela indique une relation positive modérée entre l'âge et la présence de la maladie cardiaque. En d'autres termes, à mesure que l'âge augmente, la probabilité de développer une maladie cardiaque semble également augmenter de manière modérée.       

cor.test(df$MaladieCardiaque,df$Sexe)

#==>Le coefficient de corrélation de Pearson (r) entre "MaladieCardiaque" et "Sexe" est de 0.305. Cela suggère une relation positive modérée entre le sexe et la probabilité de développer une maladie cardiaque. En d'autres termes, il existe une corrélation modérée entre le sexe et la prévalence de la maladie cardiaque.       

cor.test(df$MaladieCardiaque,df$TypeDouleurThoracique)

#==> La corrélation de Pearson entre "MaladieCardiaque" et "TypeDouleurThoracique" révèle une corrélation significative et négative (r = -0.382). Cela suggère une relation modérée où certains types de douleur thoracique sont associés à une diminution modérée de la probabilité de développer une maladie cardiaque.       

cor.test(df$MaladieCardiaque,df$TensionArterielleRepos)
#==> La corrélation de Pearson entre "MaladieCardiaque" et "TensionArterielleRepos" est faible mais positive (r = 0.113). Ceci suggère une relation légère entre la tension artérielle au repos et la probabilité de développer une maladie cardiaque.        

cor.test(df$MaladieCardiaque,df$Cholesterol)
#==> La corrélation de Pearson entre "MaladieCardiaque" et "Cholesterol" est faible mais positive (r = 0.069). Cela indique une relation légère entre le taux de cholestérol et la probabilité de développer une maladie cardiaque.       

cor.test(df$MaladieCardiaque,df$GlycemieJeune)
#==> La corrélation de Pearson entre "MaladieCardiaque" et "GlycemieJeune" est significative et positive (r = 0.266). Cela suggère une relation modérée entre la glycémie à jeun et la probabilité de développer une maladie cardiaque.        

cor.test(df$MaladieCardiaque,df$ECGRepos)
#==> La corrélation de Pearson entre "MaladieCardiaque" et "ECGRepos" est faible et non significative (r = 0.059, p = 0.078). Cela indique une relation légère entre les résultats de l'ECG au repos et la probabilité de développer une maladie cardiaque, mais cette corrélation n'est pas statistiquement valide.       

cor.test(df$MaladieCardiaque,df$FrequenceCardiaqueMax)

#==>La corrélation de Pearson entre "MaladieCardiaque" et "FrequenceCardiaqueMax" est significative et négative (r = -0.368). Cela indique une relation modérée négative entre la fréquence cardiaque maximale et la probabilité de développer une maladie cardiaque.       

cor.test(df$MaladieCardiaque,df$AngineExercice)
#==>La corrélation de Pearson entre "MaladieCardiaque" et "AngineExercice" est significative et positive (r = 0.492). Cela indique une corrélation modérée à forte entre l'angine induite par l'exercice et la probabilité de développer une maladie cardiaque.       

cor.test(df$MaladieCardiaque,df$DepressionAncienne)
#==>Cette corrélation de Pearson entre "MaladieCardiaque" et "DepressionAncienne" indique une corrélation significative et positive (r = 0.393). Cela suggère une relation modérée entre l'histoire de dépression et la probabilité de développer une maladie cardiaque.        

cor.test(df$MaladieCardiaque,df$PenteSTExercice)

#==>Cette corrélation de Pearson entre "MaladieCardiaque" et "PenteSTExercice" indique une corrélation significative et négative (r = -0.549). Cela suggère une relation modérée à forte entre la pente de l'onde ST à l'exercice et la probabilité de développer une maladie cardiaque.       



#----------------------------------------------APPLICATION DE LA REGRESSION LOGISTIQUE-----------------------------------------------       

model = glm(formula = MaladieCardiaque~ Age+Sexe+TypeDouleurThoracique+FrequenceCardiaqueMax+AngineExercice+DepressionAncienne+PenteSTExercice,data=df)
plot(model)
summary(model)

#==> Les variables explicatives significatives pour prédire MaladieCardiaque sont :        

 #Âge, Sexe, TypeDouleurThoracique, FrequenceCardiaqueMax, AngineExercice, DepressionAncienne, PenteSTExercice       
 

#Interprétation des coefficients :       
 #Âge : Pour chaque unité d'augmentation de l'âge, le log-odds de MaladieCardiaque augmente de 0.21734.        
 
 #Sexe : Chaque unité de changement dans la variable binaire "Sexe" entraîne une augmentation de 0.20975 dans le log-odds.       
 
 #TypeDouleurThoracique : Chaque unité de changement dans cette variable diminue le log-odds de MaladieCardiaque de 0.09734.        
 

 #Toutes les variables ont une significativité statistique pour prédire la MaladieCardiaque.       
 

 #La qualité du modèle semble bonne avec une deviance significative et un AIC de 730.51.       
 

 #Conclusion : Le modèle est bien ajusté avec des variables significatives pour prédire la probabilité de MaladieCardiaque.       
 

install.packages('MASS')
library(MASS)
 #Création de l'analyse discriminante linéaire       
 
lda_model <- lda(MaladieCardiaque~ Age+Sexe+TypeDouleurThoracique+FrequenceCardiaqueMax+AngineExercice+DepressionAncienne+PenteSTExercice, data = df)


 #Résumé des résultats       
summary(lda_model)
#application du model lda :        
set.seed(123)
 #Division du dataset en 'train' et 'test'      
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]
 #Prediction sur le test :
predicted <- predict(lda_model, newdata = test_data)

predicted$class

#Evaluation du model lda :       

 #Confusion Matrix       
conf_matrix <- table(test_data$MaladieCardiaque, predicted$class)
conf_matrix
 #Accuracy       
 
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
 #Precision        
precision <- diag(conf_matrix) / rowSums(conf_matrix)
precision
 #Recall (Sensitivity)       
recall <- diag(conf_matrix) / colSums(conf_matrix)
recall
 #F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
#Ce résultat de la matrice de confusion montre que :       

#Vrais négatifs (TN) : 101 : Il y a 101 cas correctement prédits comme n'appartenant pas à la classe "maladie cardiaque absente".       

#Vrais positifs (TP) : 129 : 129 cas ont été correctement prédits comme appartenant à la classe "maladie cardiaque présente".        

#Faux négatifs (FN) : 24 : 24 cas ont été prédits comme n'appartenant pas à la classe "maladie cardiaque absente", alors qu'ils appartiennent à cette classe.        

#Faux positifs (FP) : 16 : 16 cas ont été prédits comme appartenant à la classe "maladie cardiaque présente", alors qu'ils n'appartiennent pas à cette classe.        

#Les métriques de performance :        

#Précision : Environ 80.8% pour la classe "absente" et environ 89% pour la classe "présente". Cela représente la proportion d'observations correctement identifiées pour chaque classe.        

#Sensibilité (Rappel) : Environ 86.3% pour la classe "absente" et environ 84.3% pour la classe "présente". Cela représente la capacité du modèle à identifier correctement les vrais positifs.        

#F1-score : Environ 83.5% pour la classe "absente" et environ 86.6% pour la classe "présente". C'est une mesure de la précision et de la sensibilité, prenant en compte l'harmonique entre ces deux métriques.       


