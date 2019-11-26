Classmates<-c("Àlex", "Alessandro", "Alejandro", "Kevin", "Sergi", "Laura", "Fabi", "David", "Jasper","Oriol")
set.seed(123)
df<-split(Classmates, sample(Classmates, 5))
df
View(df)
