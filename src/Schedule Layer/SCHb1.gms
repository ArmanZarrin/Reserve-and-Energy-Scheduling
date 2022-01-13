sets
T        hours           /t1*t24/        ,
i        Gen Index       /1*3/           ,
*y(i)     Gen (0 1)       /1*2/           ,
j        DG index        /j1*j2/         ,
n        buses           /1*14/          ,
x        counter         /1*12/          ,
*k        scen indices    /k1*k100000000/ ,
*y        FOR index       /1*2/           ,
o        order index     /1*50/          ,
t1       orders          /1*24/          ,
k        anva            /1*3/
;
alias(n,node);
alias(x,x1);
alias(x,x2);
*alias(y,y1);
*alias(y,y2);
*________________________________________________________

*________________________________________________________
scalar
loaddev  /0.03/  ,
PCC      /1/     ,
PVbus    /5/     ,
Windbus  /10/    ,
PVdev    /0.05/  ,
Winddev  /0.1/   ,
pbatmax  /40/    ,
B1       /10/   ,
B2       /0.05/   ,
VR       /1/     ,
EtaCh    /0.86/  ,
Etadis   /0.86/  ,
SBatmin   /0.2/   ,
Sbatmax   /0.8/   ,
dt       /5/     ,
Ebat     /200/   ,
sbat0    /0.5/   ,
Rbase    /0.64/  ,
Xbase    /0.1/   ,
Zbase    /40/     ,
sbase    /100/   ,
tiecap   /100/   ,
* Vbase=¿¿ , Sbase=100kw
M                /1000000/
;
*________________________________________________________
parameter PDG(t,j);
$CALL GDXXRW.EXE data1.xlsx par=PDG rng=DGs!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD PDG

scalar f;
$GDXIN MtoG
$LOAD f
$GDXIN
*PV DGs
PDG(t,'j1')=PDG(t,'j1')*f;
*Wind DGs
PDG(t,'j2')=PDG(t,'j2')*f;

*________________________________________________________
parameter Nprob(x,*);
$CALL GDXXRW.EXE data1.xlsx par=Nprob rng=Dist!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD Nprob
*________________________________________________________
parameter branch(n,n,*);
$CALL GDXXRW.EXE data1.xlsx par=branch rng=Branch!a1 rdim=2 cdim=1
$GDXIN data1.gdx
$LOAD branch
branch(n,node,'d')$(branch(node,n,'d'))=branch(node,n,'d');
branch(n,node,'r')$(branch(node,n,'r'))=branch(node,n,'r');
branch(n,node,'x')$(branch(node,n,'x'))=branch(node,n,'x');
branch(n,node,'r')=branch(n,node,'r')/Zbase;
branch(n,node,'x')=branch(n,node,'x')/Zbase;
display branch

*________________________________________________________
parameter gen(i,*);
$CALL GDXXRW.EXE data1.xlsx par=gen rng=gens!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD gen

*parameter Gprob(i,y);
*Gprob(i,'1')= gen(i,'FORate');
*Gprob(i,'2')=1-gen(i,'FORate');
*________________________________________________________
parameter price(t,*);
$CALL GDXXRW.EXE data1.xlsx par=price rng=prices!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD price
*________________________________________________________
parameter LProf(t,*);
$CALL GDXXRW.EXE data1.xlsx par=LProf rng=Load_profile!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD LProf
*________________________________________________________
parameter PfixL(n,*);
$CALL GDXXRW.EXE data1.xlsx par=PfixL rng=Loads!a1 rdim=1 cdim=1
$GDXIN data1.gdx
$LOAD PfixL

parameter PL(n,t,*);
PL(n,t,'L1')=PfixL(n,'L1')*LProf(t,'L1');
PL(n,t,'L2')=PfixL(n,'L2')*LProf(t,'L2');
parameter PLt;
parameter QLt;
PLt(t)=sum(n,PL(n,t,'L1')+PL(n,t,'L2'));
QLt(t)=sum(n,(PL(n,t,'L1')+PL(n,t,'L2'))*tan(arccos(PfixL(n,'PF'))))
parameter PD(n,t);
parameter Qd(n,t);
PD(n,t)=PL(n,t,'L1')+PL(n,t,'L2');
Qd(n,t)=(PL(n,t,'L1')+PL(n,t,'L2'))*tan(arccos(PfixL(n,'PF')));
PD(n,t)=PD(n,t)/sbase;
Qd(n,t)=Qd(n,t)/sbase;


parameter MTPD;
$GDXIN MtoG5
$LOAD MTPD
$GDXIN
display MTPD

parameter market;
market(t,o,'pmax')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'1',t1)$(MTPD(o,'1',t1)>0));
market(t,o,'pmin')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'1',t1)$(MTPD(o,'1',t1)<0));
market(t,o,'cost')=sum(t1$(ord(t1)=ord(t)),MTPD(o,'2',t1));
display market
*________________________________________________________
parameter ak(x,x1,x2);
ak(x,x1,x2)= Nprob(x,'prob')*Nprob(x1,'prob')*Nprob(x2,'prob');

*________________________________________________________
binary variable
Bat(t);
positive variable
CVd(t)           voltage deviation cost          ,
CBat             battery cost                    ,
CDG(t,i)       Fuel cost                       ,
Ccurt(t)         curtail cost                    ,
Pch(t)           bat charge power                ,
pdis(t)          bat discharge power             ,
Vdminpos(t)                                      ,
Vdminneg(t)                                      ,
Vdmaxpos(t)                                      ,
Vdmaxneg(t)                                      ,
PGenpos(t,i)                                     ,
PGenneg(t,i)                                     ,
Qtie(t)                                          ,
Ptiepos(t)                                       ,
Ptieneg(t)                                       ,
Rdgui(t,i)                                       ,
Rdgu(t)                                  ,
maxRup(t)                                        ,
minRup(t)                                         ,
ELNS(t)                                           ,
LNS(n,t)                                           ,
Rdgdi(t,i)                                       ,
Rdgd(t)                                  ,
maxRdn(t)                                        ,
minRdn(t)                                         ,
EPNU(t)                                           ,
PNU(n,t)                                           ,
CBatpos                                          ,
CBatneg                                          ,
LOLEk1(t,x,x1,x2)                        ,
LOLEk2(t,x,x1,x2)                        ,
PNUEk1(t,x,x1,x2)                        ,
PNUEk2(t,x,x1,x2)                        ,
Crisk(t)                                         ,
Cmarket(t)

                        ;
Variables
COM(t)           Opearation & Maintenance cost   ,
Cs               schedule cost                   ,
Ctie(t)          tie cost                        ,
PGen(t,i)        generator power                 ,
ptie(t)          tie power                       ,
Vmin(t)          minimum voltage bus             ,
Vmax(t)          maximum voltage bus             ,
Sbat(T)          state of charge                 ,
V(t,n)          nodes voltage                    ,
P(t,n,node)      LF var                          ,
Q(t,n,node)      LF var                          ,
Pg(n,t)                                          ,
Qg(n,t)                                          ,

PGap(t,x,x1,x2)                         ,
LOLEk(t,x,x1,x2)                         ,
PNUEk(t,x,x1,x2)                         ,
Pmarket(t,o,k)
;


Pch.up(t)=pbatmax;
pdis.up(t)=pbatmax;
*pch.up(t)=0;
*pdis.lo(t)=0;
ptie.lo(t)=-tiecap;
ptie.up(t)=tiecap;
*Vmin.fx(t)=1;
Sbat.up(t)=Sbatmax;
Sbat.lo(t)=Sbatmin;
*PGen.fx('3','t1')=40;
*Sbat.fx('t24')=sbat0;
*Cbat.fx(t)=0;
*PGen.lo('t7','1')=10;

V.lo(t,n)=0.9;
V.up(t,n)=1.1;
*V.fx(t,'1')=1.05;
*pdis.fx('t9')=8;
*pch.fx('t9')=0;
*pgen.fx('t21','1')=120;
*Pmarket.fx(t,o,'1')=0;
PMdn.up(t,o,x,x1,x2)=0;
PMup.lo(t,o,x,x1,x2)=0;
*________________________________________________________
Equation
eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9,eq10,eq11,eq12,eq13,eq14,eq15,eq16,eq17,eq18,eq19,eq20,eq21,eq22,eq23,eq24,eq25,eq26,eq27,eq28,eq29,eq30,eq31,eq32,eq33,eq34,eq35,eq36,eq37,eq38,eq39,eq40,eq41,eq42,eq43,eq44,eq45,eq46,eq47,eq48,eq49,eq50,eq51,eq52,eq53,eq54,eq55,eq56,eq57,eq58;


eq1                                      .. Cs=e=sum(t,COM(t)+CVd(t)+Cmarket(t))+CBat+sum(t,Crisk(t))+sum(t,Ccurt(t));
eq2(t)                                   .. COM(t)=e=sum(i,CDG(t,i))+Ctie(t)+Ccurt(t);
eq3(t,i)                                 .. CDG(t,i)=e=Gen(i,'C_M')*(PGenpos(t,i)+PGenneg(t,i));
eq4(t)                                   .. ccurt(t)=e=3*price(t,'P')*sum(n,lns(n,t))+0.5*price(t,'P')*sum(n,pnu(n,t));
eq5(t)                                   .. Crisk(t)=e=3*price(t,'P')*Elns(t)+0.5*price(t,'P')*Epnu(t);
eq6(t)                                   .. Cmarket(t)=e= sum(o,sum(k,Pmarket(t,o,k))*market(t,o,'cost'));
eq7(o,t)                                 .. sum(k,Pmarket(t,o,k))=g= market(t,o,'Pmin');
eq8(o,t)                                 .. sum(k,Pmarket(t,o,k))=l= market(t,o,'Pmax');
eq55(k,o,t)                              .. Pmarket(t,o,k)=g= market(t,o,'Pmin');
eq56(k,o,t)                              .. Pmarket(t,o,k)=l= market(t,o,'Pmax');
eq9(t,i)                                 .. PGen(t,i)=e=PGenpos(t,i)-PGenneg(t,i);
eq10(t)                                  .. ctie(t)=e=price(t,'P')*Ptiepos(t)-price(t,'S')*Ptieneg(t);
eq11(t)                                  .. ptie(t)=e=Ptiepos(t)-Ptieneg(t);
eq12(t)                                  .. CVd(t)=e=B1*(Vdminpos(t)+Vdminneg(t)+Vdmaxpos(t)+Vdmaxneg(t));
eq13(t)                                  .. 1-(Vmin(t)/VR)=e=Vdminpos(t)-Vdminneg(t);
eq14(t)                                  .. (Vmax(t)/VR)-1=e=Vdmaxpos(t)-Vdmaxneg(t);
eq15                                     .. CBat=e=B2*(CBatpos+CBatneg);
eq16                                     .. sum(t,Pdis(t)/Etadis-EtaCh*Pch(t))=e= CBatpos-CBatneg;
eq17(t)$(ord(t)>1)                       .. Sbat(t)=e=Sbat(t-1)-(Pdis(t)/etadis-etach*Pch(t))/Ebat ;
eq18(t)$(ord(t)=1)                       .. Sbat(t)=e=Sbat0-(Pdis(t)/etadis-etach*Pch(t))/Ebat ;
eq19(t)                                  .. PGen(t,'1')=e=Pdis(t)-Pch(t);
eq20(t)$(ord(t)>1)                       .. Pdis(t)=l=(Sbat(t-1)-Sbatmin)*Ebat*etadis;
eq21(t)$(ord(t)>1)                       .. Pch(t)=l= (Sbatmax-Sbat(t-1))*Ebat/etach;
eq22(t)$(ord(t)=1)                       .. Pdis(t)=l=(Sbat0-Sbatmin)*Ebat*etadis;
eq23(t)$(ord(t)=1)                       .. Pch(t)=l= (Sbatmax-Sbat0)*Ebat/etach;


eq24(t,i)$(ord(t)>1)                     .. PGen(t,i)=l=PGen(t-1,i)+Gen(i,'Rup')*dt;
eq25(t,i)$(ord(t)>1)                     .. PGen(t,i)=g=PGen(t-1,i)-Gen(i,'Rdn')*dt;
eq26(t,i)                                .. PGen(t,i)=g=Gen(i,'PGenmin');
eq27(t,i)                                .. PGen(t,i)=l=Gen(i,'PGenmax');
*loadflow_equations
eq28(t,n)                                .. PG(n,t)=e=(PDG(t,'j1')$(ord(n)=PVbus)+PDG(t,'j2')$(ord(n)=Windbus)+PGen(t,'1')$(ord(n)=gen('1','bus'))+PGen(t,'2')$(ord(n)=gen('2','bus'))+PGen(t,'3')$(ord(n)=gen('3','bus'))+(ptie(t)+sum(o,Pmarket(t,o,'1')))$(ord(n)=PCC))/sbase;
eq29(t,n)                                .. QG(n,t)=e=(Qtie(t)/sbase)$(ord(n)=1);
eq30(t,n)                                .. PG(n,t)-PD(n,t)+(lns(n,t)-Pnu(n,t))/sbase=e= sum(node$(branch(n,node,'d')),P(t,n,node));
eq31(t,n)                                .. QG(n,t)-QD(n,t)=e= sum(node$(branch(n,node,'d')),Q(t,n,node));
eq32(t,n,node)$(branch(n,node,'d')<>0)   .. P(t,n,node)+P(t,node,n)=e=0 ;
eq33(t,n,node)$(branch(n,node,'d')<>0)   .. Q(t,n,node)+Q(t,node,n)=e=0 ;
eq34(t,n,node)$(branch(n,node,'d') and (ord(n)<ord(node)) )   .. V(t,n)-V(t,node)=e=branch(n,node,'r')*P(t,n,node)+branch(n,node,'x')*Q(t,n,node);
eq35(t,n)                                .. Vmin(t)=l=V(t,n);
eq36(t,n)                                .. Vmax(t)=g=V(t,n);
eq37(t,x,x1,x2)                          .. PGap(t,x,x1,x2)=e= (Nprob(x,'C')*0.06*sum(n,PL(n,t,'L1')+PL(n,t,'L2')))/3 - Nprob(x1,'C')*0.1*PDG(t,'j1')/3 - Nprob(x2,'C')*0.2*PDG(t,'j2')/3 + PGen(t,'1') $(ord(y)=1)+ PGen(t,'2')$(ord(y1)=1) + PGen(t,'3')$(ord(y2)=1)+Gen('1','Rup')$(ord(y)=1)+Gen('2','Rup')$(ord(y1)=1)+Gen('3','Rup')$(ord(y2)=1)-sum(n,Pnu(n,t))+sum(n,lns(n,t));
eq38(t,i)                                .. Rdgui(t,i)=l=Gen(i,'Rup')*dt;
eq39(t,i)                                .. Rdgui(t,i)=l=Gen(i,'Pgenmax')-PGen(t,i);
eq40(t)                                  .. Rdgu(t)=e=Rdgui(t,'1')$(ord(y)=2)+Rdgui(t,'2')$(ord(y1)=2)+Rdgui(t,'3')$(ord(y2)=2);
*eq38(t,x,x1,x2)                         .. PGapd(t,x,x1,x2)=e= (Nprob(x,'A')*0.06*sum(n,PL(n,t,'L1')+PL(n,t,'L2')))/3 - Nprob(x1,'B')*0.1*PDG(t,'j1')/3 - Nprob(x2,'B')*0.2*PDG(t,'j2')/3 + PGen(t,'1') $(ord(y)=1)+ PGen(t,'2')$(ord(y1)=1) + PGen(t,'3')$(ord(y2)=1)+Gen('1','Rup')$(ord(y)=1)+Gen('2','Rup')$(ord(y1)=1)+Gen('3','Rup')$(ord(y2)=1);
eq41(t,i)                                .. Rdgdi(t,i)=l=Gen(i,'Rdn')*dt;
eq42(t,i)                                .. Rdgdi(t,i)=l=PGen(t,i)-Gen(i,'Pgenmin');
eq43(t)                                  .. Rdgd(t)=e=Rdgdi(t,'1')$(ord(y)=2)+Rdgdi(t,'2')$(ord(y1)=2)+Rdgdi(t,'3')$(ord(y2)=2);

eq44(t,x,x1,x2)                          .. LOLEk(t,x,x1,x2)=e= PGap(t,x,x1,x2)-Rdgu(t)-sum(o,Pmarket(t,o,'2'));
eq45(t,x,x1,x2)                          .. LOLEk(t,x,x1,x2)=e=LOLEk1(t,x,x1,x2)-LOLEk2(t,x,x1,x2);
eq46(t)                                  .. ELNS(t)=e=sum((x,x1,x2),LOLEk1(t,x,x1,x2)*ak(x,x1,x2));
eq47(t,x,x1,x2)                          .. PNUEk(t,x,x1,x2)=e= -PGap(t,x,x1,x2)-Rdgd(t)+sum(o,Pmarket(t,o,'3'));
eq48(t,x,x1,x2)                          .. PNUEk(t,x,x1,x2)=e=PNUEk1(t,x,x1,x2)-PNUEk2(t,x,x1,x2);
eq49(t)                                  .. EPNU(t)=e=sum((x,x1,x2),PNUEk1(t,x,x1,x2)*ak(x,x1,x2));

eq50(n,t)                                .. pnu(n,t)=l=(1-(PVdev$(ord(n)=PVbus)+Winddev$(ord(n)=Windbus)))*(PDG(t,'j1')$(ord(n)=PVbus)+PDG(t,'j2')$(ord(n)=Windbus));
eq51(t,i)$(ord(i)=1)                     .. Rdgui(t,i)+PGen(t,i)=l=(Sbat(t)-Sbatmin)*Ebat*etadis;
eq52(t,i)$(ord(i)=1)                     .. PGen(t,i)-Rdgdi(t,i)=g=(Sbat(t)-Sbatmax)*Ebat/etach;
eq53(t,n)                                .. lns(n,t)=l=(1-loaddev)*PD(n,t)*sbase;
eq54(t)                                  .. Pgenpos(t,'1')=e=pdis(t);
eq57(t)                                  .. Pdis(t)=l=Bat(t)*M;
eq58(t)                                  .. Pch(t)=l=(1-Bat(t))*M;

*_______________________________________________________________________________

model main1 /all/;
*option minlp=alphaecp;
option limrow=500;
solve main1 us lp min Cs;


display Cs.l,PGen.l,ptie.l,PDG,PD,QD,branch,pLt,Qlt,PDG;
parameter utie(t);
utie(t)=0;

utie(t)$(ptie.l(t)>0)=1;


*display maxrup,minrup,pgapu;
parameter Disinput(t,*);
Disinput(t,'PBAT')=PGen.l(t,'1');
Disinput(t,'PDE')=PGen.l(t,'2');
Disinput(t,'PMT')=PGen.l(t,'3');
Disinput(t,'PTie')=ptie.l(t);
Disinput(t,'uTie')=utie(t);
Disinput(t,'Sbat_sch')= Sbat.l(t);
parameter Slns(n,t),Spnu(n,t);
Spnu(n,t)=pnu.l(n,t);
Slns(n,t)=lns.l(n,t);
execute_unload 'sch_out1.gdx';
execute 'GDXXRW sch_out1.gdx par=Disinput rng=sheet1!a1';
parameter Csval,cDGval,ctieval,cvdval,Ccurtval,CBatval,pnuval,lnsval,rampup,rampdn,PGenold(t,i),Sbatold(t),flag,Cmarketval,ptieval(t);
ptieval(t)=ptie.l(t);
Sbatold(t)=Sbat.l(t);
PGenold(t,i)=PGen.l(t,i);
rampup= sum(t,Rdgu.l(t));
rampdn= sum(t,Rdgu.l(t));
Csval=sum(t,COM.l(t));
lnsval=sum((n,t),lns.l(n,t));
pnuval=sum((n,t),pnu.l(n,t));
CDGval=sum((t,i),cDG.l(t,i));
ctieval=sum(t,ctie.l(t));
cvdval=sum(t,cvd.l(t));
Cmarketval=sum(t,Cmarket.l(t));
CBatval=CBat.l;
Ccurtval=sum(t,Ccurt.l(t));
flag=main1.modelstat;
scalar k1;
for(k1=1 to card(t),
if(sum(t$(ord(t)=k1),pch.l(t)*pdis.l(t)) ne 0,
flag=50;
);
);
display csval,cDGval,ctieval,cvdval,CBatval,Ccurtval,pnuval,lnsval,flag,Cmarketval;
execute_unload 'GtoM' csval,cDGval,ctieval,cvdval,Ccurtval,CBatval,pnuval,lnsval,flag;
execute_unload 'schout' PGenold,Sbatold,ptieval;

