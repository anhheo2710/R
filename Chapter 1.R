#### 1.1 nhap du lieu
x <- c(4,2,0,5,8)
x
x[2]
x[1]
x[2:3]
x[c(1,2)]
x[c(F,T,T)]
x[x>1]
x[1]^x[2]
x[4]%%x[2]
x[4]%/%x[2]
GT<-c("F","M","M","F")
GT

DuLieu<-data.frame(Ten=c("Hieu","Thi","Trung"),GioiTinh=c("Nam","Nu","Nam"),ChieuCao=c(1.75,1.55,1.80),CanNang=c(67,70,55))
DuLieu
DuLieu$Ten
DuLieu$GioiTinh
DuLieu$ChieuCao

Ten=c("Hieu","Thi","Trung")
GioiTinh=c("Nam","Nu","Nam")
ChieuCao=c(1.75,1.55,1.80)
CanNang=c(67,70,55)
DuLieuB<-data.frame(Ten,GioiTinh,ChieuCao,CanNang)
DuLieuB

x<-c(1,0,3,0,5)
y<-c(0,2,0,4,0)
x+y
x*y
exp(x)
exp(y)
exp(x+y)
exp(x*y)
exp(x)*exp(y)

length(c(1,2,3,4,5,6))
x<-c(1,0,3,2,4)
sort(x,decreasing=TRUE)
sort(x)
x
sort(x,decreasing = TRUE)
?sort
?order
order(x)
x[order(x)]
rev(x)
sum(x)

	data<-read.csv(file.choose())
	#attach(data) : de goi lenh T1 ma khong can data$T1  # detach(data) : phuc hoi lai nhu cũ 
	data

str(data) #xem cau trúc của data 
z<-data$T1
#z<-T1
#z
dim(data) #chiều data
nrow(data)
ncol(data)
save(data,file="data1.rda") #session de cai dat thu muc mac dinh de luu data 
load("data1.rda")
###BIEN DINH TINH
#Tao bang tan so cho 1 bien dinh tinh va ve do thi hinh quat
KV
TanSoKV<-table(KV)
TanSoKV
pie(TanSoKV)
#Tao 1 bien dinh tinh tu 1 bien dinh luong
DanhGiaT1<-T1
DanhGiaT1
DanhGiaT1[T1<5]<-"Kem"
DanhGiaT1
DanhGiaT1[T1>=5 & T1<7]<-"Trung Binh"
DanhGiaT1[T1>=7 &T1 <8 ]<-"Kha"
DanhGiaT1[T1>=8]<-"Gioi"
DanhGiaT1

#Sau do tao bang ta so va do thi hinh quat
TanSoT1<-table(DanhGiaT1)
TanSoT1
pie(TanSoT1)

#Ve do thi cot
barplot(TanSoKV, main="Phan bo theo khu vuc") #dang dung
barplot(TanSoKV,horiz=TRUE,main="Phan bo theo khu vuc ") #dang nam ngang

#Do thi cot cho khu vuc va gioi tinh
table(GT,KV)
barplot(table(GT,KV),main="Phan bo nam va nu trong cac khu vuc ") #do thi cot chong

#Do thi cot ke
barplot(table(GT,KV),beside=TRUE,main="Phan bo nam va nu trong cac khu vuc ") 
barplot(table(DanhGiaT1,KV),beside=TRUE,legend.text = c("Gioi","Kem","Kha","Trung Binh")) 

###Bien Dinh Luong

#Do thi tan so. tan suat
hist(T1,main="Bieu do tan so diem toan hoc ky 1 nam lop 10",xlab="Diem" , ylab="Tan So")
hist(T1,freq=FALSE,main="bIEU DO TAN SUAT DIEM TOAN HOC KI 1 nam lop 10" , xlab="Diem" , ylab="Mat do")
?hist

#Do thi canh la
stem(T1)

############1.3 Mo ta du lieu 1 bien bang phuong phap so 
#Mode cua bien T1
TanSoT1<-table(T1)
names(TanSoT1)[TanSoT1==max(TanSoT1)]
max(TanSoT1)

median(T1)
mean(T1)

#Tu viet lenh de tinh trung vi
length(T1)
SapXepT1<-sort(T1)
median<-(SapXepT1[50]+SapXepT1[51])/2
median
#Tu tinh trung binh
mean<-sum(T1)/length(T1)
mean
#Dac trung do xu huong bien thien
max(T1)-min(T1)
#hoac dung lenh range
range(T1)
min(T1)
max(T1)
range(T1)[2]-range(T1)[1]

#Tinh phan vi
quantile(T1,probs=c(0.25,0.5,0.75))
#Tinh phuong sai, do lech chuan, he so bien thien
var(T1)
sqrt(var(T1)) # hoac dung lenh sd
sd(T1)
sqrt(var(T1))/abs(mean(T1)) #he so bien thien
##Do thi Boxplot
boxplot(T1,data=data)
##Do thi dang hop mo rong
boxplot(T1~KV,data=data,main="Diem Toan hoc ky 1",xlab="So sanh giua cac khu vuc")
boxplot(T1~GT,data=data,main="Diem Toan hoc ky 1",xlab="So sanh gioi tinh")

#########1.4 Mo ta du lieu nhieu bien
KVinGT<-table(KV,GT) #tao bang ta so cua hai bien dinh tih
KVinGT

GTinKV<-table(GT,KV)
GTinKV

#VE DO THI COT CHO HAI BIEN DINH TINH
barplot(KVinGT)
barplot(GTinKV)
barplot(KVinGT,beside=TRUE) #do thi cot ke
barplot(GTinKV,beside=TRUE) #do thi cot ke

#DO THI CHO HAI BIEN DINH LUONG
plot(T1,T2)
#he so tuong quan (THUONG DUOC DUNG NHIEU HON HIEP PHUONG SAI VI DA DUOC CHUAN HOA )
cor(T1,T2)
#hiep phuong sai
cov(T1,T2)

#Do thi cho 2 bien dinh luong
plot(T1,V1)
plot(T1,N1)


