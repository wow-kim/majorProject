proc import datafile="C:\Users\ad\Desktop\강의자료4-1\회귀분석2\회귀 기말프로젝트\heart.csv" out=heart replace;run;

proc corr data=heart;
run;

proc means data=heart;
run;

proc logistic data=heart ;
model target = age;
run;
proc logistic data=heart;
model target = sex ;
run;
proc logistic data=heart;
class cp;
model target = cp;
run;
proc logistic data=heart;
model target = trestbps;
run;
proc logistic data=heart;
model target = chol;
run;
proc logistic data=heart;
model target = fbs;
run;
proc logistic data=heart;
model target = thalach;
run;

proc logistic data=heart;
class cp;
model target = age sex cp trestbps chol fbs thalach;
run;

proc logistic data=heart;
class cp;
model target = age sex cp trestbps chol fbs thalach/scale=none aggregate noint selection=stepwise lackfit ;
run;

proc logistic data=heart ;
class cp;
model target=sex cp trestbps chol thalach/  rsquare clparm=wald noint lackfit;
output out = heart_out reschi=r_f resdev=r_d p=pred;
run;

proc logistic data=preds;
class cp;
model target=sex cp trestbps chol thalach;
roc pred=xp_1;
roccontrast;
run;

data heart_out;
set heart_out;
n=ranuni(8);
proc sort;
by  n;
run;

proc sort data=temp;
  by n;

  data training testing;
   set temp nobs=nobs;
   if _n_<=.7*nobs then output training;
    else output testing;
   run;

Proc Logistic Data = training;
class cp;
model target = sex cp trestbps chol thalach/ noint lackfit ctable pprob =0.5;
Output out= oo p=ppred;
Score data=testing out = Logit_File;
Run;

proc glmselect data=heart;
class cp;
model target = sex cp trestbps chol thalach;


proc sort heart_out;
by trestbps;
run;
data heart_out;
set heart_out;
index = _N_;
run;

data h;
input age sex cp trestbps chol fbs thalach target;
cards;
50 0 0 131 246 0 149 .
;
run;

proc sort data=heart;
by age;
run;
data heart;
merge heart h;
by age;
run;

data heart_out;
set heart_out;
r = rannor(123);
run;

proc sgplot data=heart_out;
scatter x=thalach y=r;
yaxis label = "Pearson Residual";
run;
proc sql;
  DELETE from heart_out
  WHERE  r_f=2;
quit;
