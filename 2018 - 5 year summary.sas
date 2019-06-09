***********************************************************************************;
* Fitting new standard curves
***********************************************************************************;
* 28/11/2016 Based on AnnualGraphsFits2016.sas;
* 10/7/2017 Based on Last5Years2016.sas;

options Mprint;
x "cd \\Pal-file\home$\HRPAJH\My Documents\@Work1819\Kiwifruit\Period1\Analysis";

%let folder = \\Pal-file\home$\HRPAJH\My Documents\@Work1819\Kiwifruit\Period1\Data\Summaries;
%let infile = &folder.FWDMSummary_all.xls;
ods _all_ close; ods listing; ods graphics off; 

%macro import_excel(set,file,range,getnames);
PROC IMPORT OUT=&set DATAFILE= "&file" DBMS=XLSX REPLACE;
 RANGE="&range";
 GETNAMES=&getnames;
RUN;
%mend import_excel;
%macro import_xls(set,file,range,getnames);
PROC IMPORT OUT=&set DATAFILE= "&file" DBMS=XLS REPLACE;
 RANGE="&range";
 GETNAMES=&getnames;
RUN;
%mend import_excel;

* read in data summaries;
%macro read_year(year);
%import_xls(raw,&folder\FWDMSummary_all.xls,&year$A11:L10000,NO);
data FWDM_&year; set raw;
 format hyear round dafb FW DM FW_sd DM_sd FW_n DW_n BEST. date date9.;
 length variety $2. grower $5.;
 hyear=A; variety=B; grower=C; round=D; date=E; dafb=F; FW=G; DM=H; FW_sd=I; DM_sd=j; FW_n=K; DW_n=L;
 if hyear~=.;
 if variety="HW" or variety="GA" or variety="HE"; * varieties of interest 2015;
 drop A B C D E F G H I J K L;
run;
proc sort data=FWDM_&year; by variety grower round;
* and orchards which go to round 10 (note change - ignoring internal "jumps");
proc means noprint data=FWDM_&year; var round; output out=x0 max=rounds;
data temporary; set x0; call symput("rounds",rounds); run;
data x; set FWDM_&year; if DM=. then round=.;
proc means noprint data=x noprint; by variety grower; var round; output out=x1 max=nDM;
data x; set FWDM_&year; if FW=. then round=.;
proc means noprint data=x noprint; by variety grower; var round; output out=x2 max=nFW;
data all&year; merge FWDM_&year x1 x2; by variety grower;
 if nDM<&rounds then DM=.;
 if nFW<&rounds then FW=.;
 keep hyear variety Grower round date dafb FW DM;
proc sort data=all&year; by variety round; 
proc means noprint data=all&year; by variety round; var dafb fw dm; output out=av_&year mean=dafb fw dm; 
proc sort data=all&year; by variety grower round; 
run;
%mend read_year;
%read_year(2018);
%read_year(2017);
%read_year(2016);
%read_year(2015);
%read_year(2014);


data all; set FWDM_2014 FWDM_2015 FWDM_2016 FWDM_2017 FWDM_2018; 
 if variety="GA" or variety="HW";
run;
proc print; run;
proc means noprint data=all; by hyear variety grower; var FW; output out=x1 n=n; run;
proc means noprint data=x1; by hyear variety; var n; output out=x2 n=n; run;
proc print; run;

* blanks for standard curves so will get predictions for every day;
data blanks; 
 do dafb=50 to 210; 
  variety="HW"; output;
  variety="GA"; output;
 end;
run;
* initially no weightings - treat every data point as equal weighting;
proc sort data=all; by variety hyear grower;
data to_fit1; set all;
 retain orch 0;
 if (variety~=lag(variety)) then orch=0;
 if (grower~=lag(grower) or hyear~=lag(hyear)) then orch+ 1;
data to_fit; set blanks to_fit1;
proc print; run;


* creates 'n' variables named m1 -mn and sets them to 1;
%macro m(n); %do j=1 %to &n; m&j=1 %end; %mend m;

* ------------------------------------------------ curve fitting macro ---------------------------------------------------;
* Fit new fresh weight curves;
%macro fit_FW(variety,Tmid3,maxdrop);
data &variety; set to_fit; if variety="&variety"; run;
data x; set &variety nobs=last; if _n_=last; call symput("n",compress(orch)); run;
%let n1=%eval(&n-1);
%put &variety &n &n1; * writes to sas log;
proc nlin data=&variety method=marquardt maxiter=500;
 parms Tmid1=38 k1=0.1 Tmid2=105 k2=0.045 A=60 %m(&n1) k3=0.05 Dmax=100; 
 Tmid3=&Tmid3; maxdrop=&maxdrop; * get Tmid3 etc. by trial and error
; bounds 35<Tmid1<40, 0.10<k1<0.12, k3>0.04;
 array m{&n};
 m&n=1;
 if round=. then m_=mean(of m1-m&n); else m_=m{orch};
 Dmin=0; B = Dmax-Dmin-A;
 pred = m_ * (Dmin + A/(1+exp(-k1*(dafb-Tmid1))) + B/(1+exp(-k2*(dafb-Tmid2)))) * ((1-maxdrop) + maxdrop/(1+exp(k3*(dafb-Tmid3))));
 model fw = pred;
 id Dmin Tmid3 maxdrop;
 output out=FWfit_&variety pred=FWpred parms=Tmid1 k1 Tmid2 k2 A m1-m&n1 k3 Dmax sse=sse;
run;
data x; set FWfit_&variety; if _n_=1; keep variety Tmid3 sse; proc print; run;
%mend fit_FW;

* ------------------------------------------------ curve fitting macro ---------------------------------------------------;

* select Tmid3 by trial-and-error for each cultivar - might require looking at graphs as well as sum of squares;
*%fit_FW(HW,240,.08); * was 230;
*%fit_FW(HW,230,.08); * was 230;
*%fit_FW(HW,220,.08); * was 230;
*%fit_FW(HW,210,.08); * was 230;
%fit_FW(HW,210,.09); * was 230;
*%fit_FW(GA,185,.08); * was 175;
*%fit_FW(GA,165,.08); * was 175;
*%fit_FW(GA,175,.08); * 13119 was 175;
*%fit_FW(GA,180,.08); * was 175;
*%fit_FW(GA,180,.07); * was 175;
*%fit_FW(GA,175,.10); * 13064 was 175;
*%fit_FW(GA,185,.10); * 13125 was 175;
%fit_FW(GA,180,.10); * 13065 was 175;

%macro plot_FW(variety);
proc sort data=FWfit_&variety; by hyear grower round;
goptions reset=all; symbol1 c=black v=circle i=join w=3; symbol2 c=red v=none i=join w=3;
legend1 frame mode=share across=1 position=(bottom right inside); 
proc gplot uniform; by hyear grower; plot fw*dafb FWpred*dafb/overlay legend=legend1; run;
%mend plot_FW;
%plot_FW(HW);
%plot_FW(GA);

* DM;
%macro m(n); %do j=1 %to &n; m&j=1 %end; %mend m;
%macro fitDM(variety);
data &variety; set to_fit; if variety="&variety"; run;
data x; set &variety nobs=last; if _n_=last; call symput("n",compress(orch)); run;
%let n1=%eval(&n-1);
%put &variety &n &n1;
proc nlin data=&variety method=marquardt maxiter=500;
 parms Tmid=100 k=0.05 Dmin=0 %m(&n1) Dmax=20
%if &variety=HW %then %do; slope=0.01 t3=200; bounds t3<220; dt3=10; %end;
 array m{&n};
 m{&n}=1;
 if round=. then m_=mean(of m1-m&n); else m_=m{orch};
%if &variety=HW %then pred = m_*(Dmin + (Dmax-Dmin)/(1+exp(-k*(dafb-Tmid))) + slope*dafb*(1-log(1+exp(-(dafb-t3)/dt3))) );
%else pred = m_*(Dmin + (Dmax-Dmin)/(1+exp(-k*(dafb-Tmid))));
 ;
 model DM = pred;
 output out=DMfit_&variety pred=DMpred parms=Tmid k Dmin m1-m&n Dmax sse=sse;
run;
data x; set DMfit_&variety; if _n_=1; keep variety sse; proc print; run;
%mend fitDM;
%fitDM(HW);
%fitDM(GA);

%macro plot_DM(variety);
proc sort data=DMfit_&variety; by hyear grower round;
goptions reset=all; symbol1 c=black v=circle i=join w=3; symbol2 c=red v=none i=join w=3;
legend1 frame mode=share across=1 position=(bottom right inside); 
proc gplot uniform; by hyear grower; plot DM*dafb DMpred*dafb/overlay legend=legend1; run;
%mend plot_DM;
%plot_DM(HW);
%plot_DM(GA);

* output unscaled standard curves;
%macro simplify(variety,v);
* macro to strip out only predicted values from fits for storage;
data &v&variety; 
 set &v.fit_&variety (rename=(&v.pred=&v._&variety)); 
 if round=.; 
 &v._&variety = round(&v._&variety,0.01);
 keep dafb &v._&variety; 
run;
%mend simplify;
%simplify(GA,DM);
%simplify(HW,DM);
%simplify(GA,FW);
%simplify(HW,FW);
data std_unscaled; merge FWHW DMHW FWGA DMGA; by dafb; run;
proc print; run;

* save to csv but remember to copy into SC_2018_unscaled sheet of StdCurvesAll.xlsx;
data _null_; set std_unscaled;
 file "SC_2018_unscaled.csv" dlm="," Lrecl=1000;
 put dafb FW_HW DM_HW FW_GA DM_GA;
run;
* scaling done in excel to create SC_2018;

**************************************************************************************;
