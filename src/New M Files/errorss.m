function [errorL1,errorL2,errorREN,Hsource]=errorss(f,MG_N,maxbus)
    for tn=24:-1:1
        for k=MG_N:-1:1
            Hsource{f,k,tn}=[0,0]; 
            for ts=12:-1:1
                errorL1{f,k,tn,ts}=0.03*randn(1,maxbus(k))/3;
                errorL2{f,k,tn,ts}=0.03*randn(1,maxbus(k))/3;
                errorREN{f,k,tn,ts}=0.05*randn(1,2)/3;
                errorREN{f,k,tn,ts}(1,2)=2*errorREN{f,k,tn,ts}(1,2);
            end
        end
    end
end