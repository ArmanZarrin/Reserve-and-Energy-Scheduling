    %clear data
clear all
clc
    %set global variables
ue={'t1' 't2' 't3' 't4' 't5' 't6' 't7' 't8' 't9' 't10' 't11' 't12' 't13' 't14' 't15' 't16' 't17' 't18' 't19' 't20' 't21' 't22' 't23' 't24'};
count=0; MG_N=3;maxbus=[14 34 37];iterschmax=14;
    %changing penetration rate with a multipiction
for f=1:0.05:1 
    count=count+1;
    
        %??
    Schedule_Layer
    
        %Generating simple errors
    [errorL1,errorL2,errorREN,Hsource]=errorss(f,MG_N,maxbus);
    save output9
end
