    %% Initilize
    
s1.name='PGenold';s1.form='full';s1.compress=false;s1.uels={ue,{[1 2 3]}};
s2.name='Sbatold';s2.form='full';s2.compress=true;
s3.name='PGensval';s3.form='full';s3.compress=false;s3.uels={[1 2 3]};
s7.name='Ptieval';s7.form='full';s7.compress=false;s7.uels={ue};

MP{MG_N}=[];
for i=1:MG_N
    MSCH{i}=zeros(24,2);
end
result(10,17,MG_N)=0;lnsd(17,MG_N)=0;pnud(17,MG_N)=0;
lnsd1(17,MG_N)=0;pnud1(17,MG_N)=0;

MTPD=0;
PGen1(MG_N,3)=0;Sbat1(1:MG_N)=0.5;itersch=0;transM=[];MSCH_tmp=[];
MSCH_tmp{MG_N}=[];KO=0;KO1=0;
    %% Level1 including 2 phases
    % Limit iterations to precision and run numbers 
while((KO(1)+KO1(1)>0.001 || itersch==0) && (itersch<=iterschmax) )
            %%% reset tmp variables
    ordsell=[];ordpur=[];porders=[];sorders=[];MSCH_tmp=[];trans=[];
    ordsell(24)=0;ordpur(24)=0;porders{24}=[];sorders{24}=[];MSCH_tmp{MG_N}=[];
    
    
    for k=1:MG_N  
            %% Phase1: schedule layer--->>>;input data & run
        iwgdx('MtoG','f:0');        
        MTPD=MP{k};
        iwgdx('MtoG5','MTPD:3');
        MSCHD=MSCH{k};
        iwgdx('MtoG6','MSCHD:2');
        Sch_layer(k);
        irgdx 'GtoM'
            %%% to find any error in GAMS
        if(flag==50) 
                Sch_layerb(k);
                irgdx 'GtoM'
                disp('b');
        elseif(flag~=1)
            disp('scherror');
            display(flag);  
        end
            %%% collecting orders for every schedule time-scale
        for t=1:24
            [ordsell(t),ordpur(t),porders{t},sorders{t}]=Addordersch(ordsell(t),ordpur(t),porders{t},sorders{t},k,t);
        end
            %%% saving the result
                % k: Microgrid index ; count: Penetration level ;
        rp=rgdx('schout',s1);PGen_sch{k}=rp.val;
        rs=rgdx('schout',s2);Sbat_sch{k}=rs.val;
        rt=rgdx('schout',s7);Ptie_sch{k}=rt.val;
        result(count,1,k)=f;
        lnss(count,k)=lnsval;pnus(count,k)=pnuval;
        result(count,4,k) =Csval;result(count,5,k)= cDGval; result(count,6,k)=ctieval;
        result(count,7,k)=cvdval;result(count,8,k)=CBatval;result(count,9,k)=Ccurtval;     
        
    end
        % phase1: *end* --> Result= orders for every microgrids
        %% Phase2: market clearing and transactions
    for t=1:24
        [marketclc,trans{t},porders{t},sorders{t}] = market(ordsell(t),ordpur(t),porders{t},sorders{t});   
        transM{t}=transMG(trans{t},MG_N);
        for k=1:MG_N
            MSCH_tmp{k}=[MSCH_tmp{k};transM{t}(k,:)];
        end
    end
    if(itersch~=0)
        for k=1:MG_N
            MSCH_tmp{k}(:,3)=MSCH_tmp{k}(:,1).*MSCH_tmp{k}(:,2);
            MSCH{k}(:,3)=MSCH{k}(:,1).*MSCH{k}(:,2);
            MSCH{k}=MSCH_tmp{k}+MSCH{k};
            MSCH{k}(:,2)=MSCH{k}(:,3)./MSCH{k}(:,1);
            MSCH{k}(:,3)=[];
            
                %%% Additional check
            for t=1:24
                if(MSCH{k}(t,1)==0)
                    MSCH{k}(t,2)=0;
                end
            end 
            
        end
    end
    %transM(:,2)=transM(:,2).*transM(:,1);
    
    %%% calculating the new difference in Market Perception
    KO=abs(sum(MSCH_tmp{1}))+abs(sum(MSCH_tmp{2}));
    KO1=abs(sum(MSCH_tmp{1}));
    itersch=itersch+1;
end
      save ('level1new')