sets
T        hours           /t1*t24/        ,
i        Gen Index       /1*3/           ,
*y(i)     Gen (0 1)       /1*2/           ,
j        DG index->j1=pv_j2=wind        /pv,wind/         ,
n        buses           /1*14/          ,
x        counter         /1*12/          ,
*k        scen indices    /k1*k100000000/ ,
*y        FOR index       /1*2/           ,
o        order index     /1*12/          ,
t1       orders          /1*24/          ,
k        anva            /1*3/            ,
SorP     order number    /purchase,sell/     ,
trans    transaction count    /1*10/         ,
auc      auction counter      /1*20/        ,
VorC     volume&cost          /V,C/
;
alias(n,node);alias(x,x1);alias(x,x2);
*alias(y,y1);alias(y,y2);
*________________________________________________________
*Setting Scalars
scalar
loaddev  /0.03/, PCC     /1/,    PVbus    /21/,
Windbus  /35/,   PVdev   /0.05/, Winddev  /0.1/,
pbatmax  /40/,   B1      /10/,   B2       /0.05/,
VR       /1/,    EtaCh   /0.86/, Etadis   /0.86/,
SBatmin  /0.2/,  Sbatmax /0.8/,  dt       /5/,
Ebat     /400/,  sbat0   /0.5/,  Rbase    /0.64/,
Xbase    /0.1/,  Zbase   /40/,   sbase    /100/,
tiecap   /200/,  M       /1000000/
;
*________________________________________________________
** Read Data from Excel
parameter PDG(t,j),Nprob(x,*),branch(n,n,*),gen(i,*),price(t,*),LProf(t,*),PfixL(n,*);
$GDXIN src/Data/data3.gdx
$LOAD PDG,Nprob,branch,gen,price,LProf,PfixL
display PDG,Nprob,branch,gen,price,LProf,PfixL
*________________________________________________________
**Read Data from Matlab
scalar f;
parameter MTPD,MSCHD;
$GDXIN MtoG
$LOAD MTPD,MSCHD,f
display MTPD,MSCHD;
*_______________________________________________________
**Data Processing

PDG(t,'pv')=PDG(t,'pv')*f;
PDG(t,'wind')=PDG(t,'wind')*f;
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
branch(n,node,'d')$(branch(node,n,'d'))=branch(node,n,'d');
branch(n,node,'r')$(branch(node,n,'r'))=branch(node,n,'r');
branch(n,node,'x')$(branch(node,n,'x'))=branch(node,n,'x');
branch(n,node,'r')=branch(n,node,'r')/Zbase;
branch(n,node,'x')=branch(n,node,'x')/Zbase;
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
parameter PL(n,t,*);
PL(n,t,'L1')=PfixL(n,'L1')*LProf(t,'L1');
PL(n,t,'L2')=PfixL(n,'L2')*LProf(t,'L2');
parameter PLt,QLt;
PLt(t)=sum(n,PL(n,t,'L1')+PL(n,t,'L2'));
QLt(t)=sum(n,(PL(n,t,'L1')+PL(n,t,'L2'))*tan(arccos(PfixL(n,'PF'))))
parameter PD(n,t),Qd(n,t);
PD(n,t)=(PL(n,t,'L1')+PL(n,t,'L2'))/sbase;
Qd(n,t)=(PL(n,t,'L1')+PL(n,t,'L2'))*tan(arccos(PfixL(n,'PF')))/sbase;
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
parameter schtrans;
schtrans(t,'V')=sum(t1$(ord(t1)=ord(t)),MSCHD(t1,'1'));
schtrans(t,'pmax')=sum(t1$(ord(t1)=ord(t)),MSCHD(t1,'1')$(MSCHD(t1,'1')>0));
schtrans(t,'pmin')=sum(t1$(ord(t1)=ord(t)),MSCHD(t1,'1')$(MSCHD(t1,'1')<0));
schtrans(t,'C')=sum(t1$(ord(t1)=ord(t)),MSCHD(t1,'2'));
display schtrans
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
parameter market;
market(t,o,'pmax')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'1',t1)$(MTPD(o,'1',t1)>0));
market(t,o,'pmin')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'1',t1)$(MTPD(o,'1',t1)<0));
market(t,o,'cost')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'2',t1));
display market
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
parameter ak(x,x1,x2);
ak(x,x1,x2)= Nprob(x,'prob')*Nprob(x1,'prob')*Nprob(x2,'prob');
*parameter Gprob(i,y);
*Gprob(i,'1')= gen(i,'FORate');
*Gprob(i,'2')=1-gen(i,'FORate');

*________________________________________________________
*Variables
positive variable
CVd(t),          CBat,           CDG(t,i),
Ccurt(t),        Pch(t),         pdis(t),
Vdminpos(t),     Vdminneg(t),    Vdmaxpos(t),
Vdmaxneg(t),     PGenpos(t,i),   PGenneg(t,i),
Qtie(t),         Ptiepos(t),     Ptieneg(t),
Rdgui(t,i),      Rdgu(t),        maxRup(t),
minRup(t),       ELNS(t),        LNS(n,t),
Rdgdi(t,i),      Rdgd(t),        maxRdn(t),
minRdn(t),       EPNU(t),        PNU(n,t),
CBatpos,         CBatneg,        LOLEk1(t,x,x1,x2),
LOLEk2(t,x,x1,x2),               PNUEk1(t,x,x1,x2),
PNUEk2(t,x,x1,x2),               Crisk(t)
;
Variables
COM(t),          Cs,             Ctie(t),        Cmarket(t),
PGen(t,i),       ptie(t),        PGap(t,x,x1,x2),        Pmarket(t,o,k),
P(t,n,node),     Q(t,n,node),    Pg(n,t),        Qg(n,t),
Vmin(t),         Vmax(t),        V(t,n),
Sbat(T),
LOLEk(t,x,x1,x2),        PNUEk(t,x,x1,x2),
PMup(t,o,x,x1,x2),       PMdn(t,o,x,x1,x2)
;


Pch.up(t)=pbatmax;pdis.up(t)=pbatmax;
ptie.lo(t)=-tiecap;ptie.up(t)=tiecap;
Sbat.up(t)=Sbatmax;Sbat.lo(t)=Sbatmin;
V.lo(t,n)=0.9;V.up(t,n)=1.1;
Pmarket.up(t,o,'1')=0;
PMdn.up(t,o,x,x1,x2)=0;PMup.lo(t,o,x,x1,x2)=0;
*________________________________________________________
Equation
eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,eq10,eq11,eq12,eq13,eq14,eq15,eq16,eq17,eq18,eq19,eq20,eq21,eq22,eq23,eq24,eq25,
eq26,eq27,eq28,eq29,eq30,eq31,eq32,eq33,eq34,eq35,eq36,eq37,eq38,eq39,eq40,eq41,eq42,eq43,eq44,eq45,eq46,eq47,eq48,
eq49,eq50,eq51,eq52,eq53,eq54,eq55,eq56,eq57,eq58;
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
***Objective Function
eq1              .. Cs=e=sum(t,COM(t)+CVd(t)+Cmarket(t))+CBat+sum(t,Crisk(t));
** Cost calculations
eq2(t)           .. COM(t)=e=sum(i,CDG(t,i))+Ctie(t)+Ccurt(t)+schtrans(t,'V')*schtrans(t,'C');
eq3(t,i)         .. CDG(t,i)=e=Gen(i,'C_M')*(PGenpos(t,i)+PGenneg(t,i));
eq4(t)           .. ccurt(t)=e=3*price(t,'P')*sum(n,lns(n,t))+0.5*price(t,'P')*sum(n,pnu(n,t));
eq5(t)           .. Crisk(t)=e=0.98*(3*price(t,'P')*Elns(t)+0.5*price(t,'P')*Epnu(t));
*        *Tieline
eq10(t)          .. ctie(t)=e=price(t,'P')*Ptiepos(t)-price(t,'S')*Ptieneg(t);
eq11(t)          .. ptie(t)=e=Ptiepos(t)-Ptieneg(t);
*        *Voltage and Battery SOC Penalties
eq12(t)          .. CVd(t)=e=B1*(Vdminpos(t)+Vdminneg(t)+Vdmaxpos(t)+Vdmaxneg(t));
eq13(t)          .. 1-(Vmin(t)/VR)=e=Vdminpos(t)-Vdminneg(t);
eq14(t)          .. (Vmax(t)/VR)-1=e=Vdmaxpos(t)-Vdmaxneg(t);
eq15             .. CBat=e=B2*(CBatpos+CBatneg);
eq16             .. sum(t,Pdis(t)/Etadis-EtaCh*Pch(t))=e= CBatpos-CBatneg;
*        **Day-Ahead Market

eq6(t)           .. Cmarket(t)=e= sum(o,Pmarket(t,o,'1')*market(t,o,'cost'))+sum((o,x,x1,x2),PMup(t,o,x,x1,x2)*ak(x,x1,x2)*market(t,o,'cost'))
                                                                         +sum((o,x,x1,x2),PMdn(t,o,x,x1,x2)*ak(x,x1,x2)*market(t,o,'cost'));
eq7(o,t)         .. sum(k,Pmarket(t,o,k))=g= market(t,o,'Pmin');
eq8(o,t)         .. sum(k,Pmarket(t,o,k))=l= market(t,o,'Pmax');
eq55(k,o,t)      .. Pmarket(t,o,k)=g= market(t,o,'Pmin');
eq56(k,o,t)      .. Pmarket(t,o,k)=l= market(t,o,'Pmax');
*        **Battery Constraint
eq17(t)$(ord(t)>1)       .. Sbat(t)=e=Sbat(t-1)-(Pdis(t)/etadis-etach*Pch(t))/Ebat ;
eq18(t)$(ord(t)=1)       .. Sbat(t)=e=Sbat0-(Pdis(t)/etadis-etach*Pch(t))/Ebat ;
eq19(t)                  .. PGen(t,'1')=e=Pdis(t)-Pch(t);
eq20(t)$(ord(t)>1)       .. Pdis(t)=l=(Sbat(t-1)-Sbatmin)*Ebat*etadis;
eq21(t)$(ord(t)>1)       .. Pch(t)=l= (Sbatmax-Sbat(t-1))*Ebat/etach;
eq22(t)$(ord(t)=1)       .. Pdis(t)=l=(Sbat0-Sbatmin)*Ebat*etadis;
eq23(t)$(ord(t)=1)       .. Pch(t)=l= (Sbatmax-Sbat0)*Ebat/etach;
eq54(t)                  .. Pgenpos(t,'1')=e=pdis(t);
eq51(t,i)$(ord(i)=1)     .. Rdgui(t,i)+PGen(t,i)=l=(Sbat(t)-Sbatmin)*Ebat*etadis;
eq52(t,i)$(ord(i)=1)     .. PGen(t,i)-Rdgdi(t,i)=g=(Sbat(t)-Sbatmax)*Ebat/etach;
*        **Conventional Power Generation Constraint
eq9(t,i)                 .. PGen(t,i)=e=PGenpos(t,i)-PGenneg(t,i);
eq24(t,i)$(ord(t)>1)     .. PGen(t,i)=l=PGen(t-1,i)+Gen(i,'Rup')*dt;
eq25(t,i)$(ord(t)>1)     .. PGen(t,i)=g=PGen(t-1,i)-Gen(i,'Rdn')*dt;
eq26(t,i)                .. PGen(t,i)=g=Gen(i,'PGenmin');
eq27(t,i)                .. PGen(t,i)=l=Gen(i,'PGenmax');
eq38(t,i)                .. Rdgui(t,i)=l=Gen(i,'Rup')*dt;
eq39(t,i)                .. Rdgui(t,i)=l=Gen(i,'Pgenmax')-PGen(t,i);
eq40(t)                  .. Rdgu(t)=e=Rdgui(t,'1')+Rdgui(t,'2')+Rdgui(t,'3');
eq41(t,i)                .. Rdgdi(t,i)=l=Gen(i,'Rdn')*dt;
eq42(t,i)                .. Rdgdi(t,i)=l=PGen(t,i)-Gen(i,'Pgenmin');
eq43(t)                  .. Rdgd(t)=e=Rdgdi(t,'1')+Rdgdi(t,'2')+Rdgdi(t,'3');
*        **Loadflow_Equations
eq28(t,n)        .. PG(n,t)=e=(PDG(t,'pv')$(ord(n)=PVbus)+PDG(t,'wind')$(ord(n)=Windbus)
                                 +PGen(t,'1')$(ord(n)=gen('1','bus'))+PGen(t,'2')$(ord(n)=gen('2','bus'))+PGen(t,'3')$(ord(n)=gen('3','bus'))
                                 +(ptie(t)+sum(o,Pmarket(t,o,'1'))+schtrans(t,'V'))$(ord(n)=PCC))/sbase;
eq29(t,n)        .. QG(n,t)=e=(Qtie(t)/sbase)$(ord(n)=1);
eq30(t,n)        .. PG(n,t)-PD(n,t)+(lns(n,t)-Pnu(n,t))/sbase=e= sum(node$(branch(n,node,'d')),P(t,n,node));
eq31(t,n)        .. QG(n,t)-QD(n,t)=e= sum(node$(branch(n,node,'d')),Q(t,n,node));
eq32(t,n,node)$(branch(n,node,'d')<>0)   .. P(t,n,node)+P(t,node,n)=e=0 ;
eq33(t,n,node)$(branch(n,node,'d')<>0)   .. Q(t,n,node)+Q(t,node,n)=e=0 ;
eq34(t,n,node)$(branch(n,node,'d') and (ord(n)<ord(node)) )   .. V(t,n)-V(t,node)=e=branch(n,node,'r')*P(t,n,node)
                                                                                 +branch(n,node,'x')*Q(t,n,node);
eq35(t,n)        .. Vmin(t)=l=V(t,n);
eq36(t,n)        .. Vmax(t)=g=V(t,n);



*        ** Scenario Calculation
eq37(t,x,x1,x2)          .. PGap(t,x,x1,x2)=e= (Nprob(x,'C')*0.06*sum(n,PL(n,t,'L1')+PL(n,t,'L2')))/3
                                 - Nprob(x1,'C')*0.1*PDG(t,'pv')/3 - Nprob(x2,'C')*0.2*PDG(t,'wind')/3
                                   -sum(n,Pnu(n,t))+sum(n,lns(n,t));
eq44(t,x,x1,x2)          .. LOLEk(t,x,x1,x2)=e= PGap(t,x,x1,x2)-Rdgu(t)-sum(o,PMup(t,o,x,x1,x2));
eq57(t,o,x,x1,x2)        .. PMup(t,o,x,x1,x2)=l=Pmarket(t,o,'2');
eq45(t,x,x1,x2)          .. LOLEk(t,x,x1,x2)=e=LOLEk1(t,x,x1,x2)-LOLEk2(t,x,x1,x2);
eq46(t)                  .. ELNS(t)=e=sum((x,x1,x2),LOLEk1(t,x,x1,x2)*ak(x,x1,x2));
eq47(t,x,x1,x2)          .. PNUEk(t,x,x1,x2)=e= -PGap(t,x,x1,x2)-Rdgd(t)+sum(o,PMdn(t,o,x,x1,x2));
eq58(t,o,x,x1,x2)        .. PMdn(t,o,x,x1,x2)=g=Pmarket(t,o,'3');
eq48(t,x,x1,x2)          .. PNUEk(t,x,x1,x2)=e=PNUEk1(t,x,x1,x2)-PNUEk2(t,x,x1,x2);
eq49(t)                  .. EPNU(t)=e=sum((x,x1,x2),PNUEk1(t,x,x1,x2)*ak(x,x1,x2));
*        **PNU and LNS constraints
eq50(n,t)                .. pnu(n,t)=l=(1-(PVdev$(ord(n)=PVbus)+Winddev$(ord(n)=Windbus)))*
                                         (PDG(t,'pv')$(ord(n)=PVbus)+PDG(t,'wind')$(ord(n)=Windbus));
eq53(t,n)                .. lns(n,t)=l=(1-loaddev)*PD(n,t)*sbase;


*_______________________________________________________________________________
**Solve Problem

model main1 /all/;
*option minlp=alphaecp;
option limrow=500;
solve main1 us lp min Cs;

*_______________________________________________________________________________
**Bidding
parameter order(t,SorP,auc,VorC);
scalar k1,k2,count;
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
* Tieline Capabality Exchange
order(t,'purchase','1','V')=Ptiepos.l(t); order(t,'purchase','1','C')= 0.99*price(t,'P');
order(t,'purchase','2','V')=tiecap-Ptieneg.l(t);order(t,'purchase','2','C')= 0.99*price(t,'S');
order(t,'sell','1','V')=ptieneg.l(t);order(t,'sell','1','C')= 1.01*price(t,'S');
order(t,'sell','2','V')=tiecap-Ptiepos.l(t);order(t,'sell','2','C')= 1.01*price(t,'P');
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
* Compensate LNS and PNU
order(t,'purchase','3','V')=sum(n,lns.l(n,t));order(t,'purchase','3','C')= 3*price(t,'P');
order(t,'sell','3','V')=sum(n,pnu.l(n,t));order(t,'sell','3','C')= -0.5*price(t,'P');
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
* Adjust Conventional Units Output
count=4;
for(k1=2 to card(i),
order(t,'purchase',auc,'V')$(ord(auc)=count)=sum(i$(ord(i)=k1),min(PGen.l(t,i)-Gen(i,'Pgenmin')-Rdgdi.l(t,i),
                                                 max(0,(Gen(i,'Rup')*dt)-(PGen.l(t+1,i)-PGen.l(t,i)))+1000$(ord(t)=24)));
order(t,'purchase',auc,'C')$(ord(auc)=count)= sum(i$(ord(i)=k1),0.99*gen(i,'C_M'));
count=count+1;
);

count=4;
for(k1=2 to card(i),
order(t,'sell',auc,'V')$(ord(auc)=count)=sum(i$(ord(i)=k1),min(Gen(i,'Pgenmax')-PGen.l(t,i)-Rdgui.l(t,i),
                                                 max(0,(Gen(i,'Rdn')*dt)-(PGen.l(t,i)-PGen.l(t+1,i)))+1000$(ord(t)=24) ));
order(t,'sell',auc,'C')$(ord(auc)=count)= sum(i$(ord(i)=k1),1.01*gen(i,'C_M'));
count=count+1;
);
*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
display order;
execute_unload 'OrderlistSch' order;

*****
display Cs.l,PGen.l,ptie.l,PDG,PD,QD,branch,pLt,Qlt,PDG;
parameter utie(t);
utie(t)=0;
utie(t)$(ptie.l(t)>0)=1;

*display maxrup,minrup,pgapu;
parameter Disinput(t,*);
Disinput(t,'PBAT')=PGen.l(t,'1');Disinput(t,'PDE')=PGen.l(t,'2');
Disinput(t,'PMT')=PGen.l(t,'3');Disinput(t,'PTie')=ptie.l(t);
Disinput(t,'uTie')=utie(t);Disinput(t,'Sbat_sch')= Sbat.l(t);
parameter Slns(n,t),Spnu(n,t);
Spnu(n,t)=pnu.l(n,t);Slns(n,t)=lns.l(n,t);
*execute_unload 'sch_out1.gdx';
execute 'GDXXRW src/GamsOut/sch_out3.gdx par=Disinput rng=sheet1!a1';
parameter Csval,cDGval,ctieval,cvdval,Ccurtval,CBatval,pnuval,lnsval,rampup,rampdn,PGenold(t,i),Sbatold(t),flag,Cmarketval,ptieval(t);
ptieval(t)=ptie.l(t);ctieval=sum(t,ctie.l(t));
Sbatold(t)=Sbat.l(t);PGenold(t,i)=PGen.l(t,i);
rampup= sum(t,Rdgu.l(t));rampdn= sum(t,Rdgu.l(t));
lnsval=sum((n,t),lns.l(n,t));pnuval=sum((n,t),pnu.l(n,t));
Csval=sum(t,COM.l(t));CDGval=sum((t,i),cDG.l(t,i));
cvdval=sum(t,cvd.l(t));CBatval=CBat.l;
Cmarketval=sum(t,Cmarket.l(t));Ccurtval=sum(t,Ccurt.l(t));

* Check Model Status
flag=main1.modelstat;
scalar k1;
for(k1=1 to card(t),
if(sum(t$(ord(t)=k1),pch.l(t)*pdis.l(t)) ne 0,
flag=50;
);
);

display csval,cDGval,ctieval,cvdval,CBatval,Ccurtval,pnuval,lnsval,flag,Cmarketval,pmarket.l;
execute_unload 'GtoM' csval,cDGval,ctieval,cvdval,Ccurtval,CBatval,pnuval,lnsval,flag;
execute_unload 'schout' PGenold,Sbatold,ptieval;
execute_unload 'Orderlist' order;


