 
#-------------------------------Chapter 1---------------------------------------


# 2.1.Mo phong bien so ngau nhien

 # Ham phat sinh bien ngua nhieen co phan phoi deu
 z<-function() runif(1) # runif(1,1,10) : phat sinh 1 so tu 1 toi 10 
 z()
 ?runif
 
 #Mo phong tung dong xu
 throw.coin<-function(){  #neu de cac lenh tren cung 1 hang thi co dau ;
   x<-runif(1)
   if(x<=1/2) 0 else 1
 }
 throw.coin()
 
 throw.coin<-function(){
   x<-runif(1)
   if(x<=1/2) "Sap" else "Ngua"
 }
 throw.coin()
 
 #tung dong xu n lan
 sample.coin<-function(n){
replicate(n,throw.coin())   
 }
 sample.coin(3)
 
 #BANG TAN SO CUA 10000 LAN TUNG
 x<-sample.coin(10000)
 table(x)
 #Tan suat
 table(x)/length(x)


# 2.2 Cac phan phoi xac suat thong dung

# 2.2.1
n <- 10000 
x <- rnorm(n)
y <- x^2
hist(y, freq = 0, breaks = 40)
curve(dchisq(x, df = 1), col = "blue", lty = 1, lwd = 2, add = TRUE)

# 2.2.2
n <- 10000
x <- rchisq(n, df = 10)
y <- rchisq(n, df = 20)
z <- x + y
hist(z, freq = 0, breaks = 40)
curve(dchisq(x, df = 30), col = "green", lty = 1, lwd = 2, add = TRUE)

# 2.2.3
n <- 10000
x <- rnorm(n)
y <- rchisq(n, df = 10)
z <- x/sqrt(y/10)
hist(z, freq = 0, breaks = 40)
curve(dt(x, df = 10), col = "blue", lty = 1, lwd = 2, add = TRUE)

# 2.2.4
n <- 10000
x <- rchisq(n, df = 10)
y <- rchisq(n, df = 30)
z <- (x/10)/(y/30)
hist(z, freq = 0, breaks = 40)
curve(df(x, df1 = 10, df2 = 30), col = "blue", lty = 1, lwd = 2, add = TRUE)





