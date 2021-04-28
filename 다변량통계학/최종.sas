proc import datafile = "C:\Users\ad\Desktop\강의자료4-1\다변량\기말프로젝트\starbucks.csv"
out= sb replace; run;

proc means data=sb;run;


proc discrim data=sb out=out_1 pool=no method=normal 
wcov pcov  listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop0 pop10 pop20 pop30 pop40 pop50 pop60 pop70  landprice;
run;

proc stepdisc data= sb;
class starbucks;
var pop0 pop10 pop20 pop30 pop40 pop50 pop60 pop70  landprice;
run;

proc discrim data=zdata out=out_2 pool=test method=normal  outstat=oo
wcov pcov listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop10 pop20 pop40 pop50 pop60 pop70 subway landprice;
run;

proc print data=oo;run;

proc standard data=sb mean=0 std=1 out=zdata;
var pop0 pop10 pop20 pop30 pop40 pop50 pop60 pop70 subway landprice;
run;

proc discrim data=zdata out=out_2z pool=no testdata=test testlist method=normal  
wcov pcov listerr crosslisterr manova ;
priors proportional ;
class starbucks;
testid dong;
var pop10 pop20 pop40 pop50 pop60 pop70 subway landprice;
run;

proc means data=zdata;run;

data test;
input dong$ pop10 pop20 pop40 pop50 pop60 pop70 subway landprice starbucks;
cards;
낙성대동 -0.441 0.502 -0.759 -0.839 -0.776 -0.774 -0.242 0.295 . 
응봉동 0.443 -0.377 0.413 -0.015 0.095 0.283 -0.517 -0.783 .
이촌2동 -0.553 -0.686 -0.239 -0.550 -0.403 0.386 0.324 -0.513 . 
;
run;

proc discrim data=zdata


proc export data=out_2z outfile="C:\Users\ad\Desktop\강의자료4-1\다변량\기말프로젝트\out.csv" dbms=csv replace;run;

proc discrim data=sb out=out_3 pool=no method=npar k=30
wcov pcov listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop0 pop10 pop20 pop30 pop40 pop50 pop60 pop70 subway landprice;
run;

proc export data=out_3 outfile="C:\Users\ad\Desktop\강의자료4-1\다변량\기말프로젝트\out.csv" dbms=csv replace;run;

proc discrim data=sb out=out_4  method=npar k=30
wcov pcov  listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop10 pop20 pop40 pop50 pop60 pop70 subway landprice;
run;

proc standard data=sb mean=0 std=1 out=zdata;
var pop0 pop10 pop20 pop30 pop40 pop50 pop60 pop70 subway landprice;
run;

proc discrim data=zdata out=out_5 pool=test method=normal slpool=0.5 
wcov pcov simple listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop10 pop20 pop40 pop50 pop60 pop70 subway landprice;
priors prop;
run;

proc discrim data=zdata out=out_6 pool=yes method=npar k=2 slpool=0.5 
wcov pcov simple listerr crosslisterr manova ;
priors proportional ;
class starbucks;
var pop10 pop20 pop40 pop50 pop60 pop70 subway landprice;
priors prop;
run;

/*---------------------------------------------------------------------------*/
