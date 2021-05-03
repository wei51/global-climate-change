%datras

url='https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/AVHRR/201201/avhrr-only-v2.20120101.nc'
path='.\1_noaa_sst_nc\2012\201201\'

%ncdisp(fullfile(path,'avhrr-only-v2.20120101.nc'));

dirs=dir(fullfile(path,'avhrr-only-v2.20120101.nc')); 
disp(dirs)

Lat=flip(ncread(fullfile(path,'avhrr-only-v2.20120101.nc'),'lat',559,50));
Lon=[ncread(fullfile(path,'avhrr-only-v2.20120101.nc'),'lon',1425,16)-360;ncread(fullfile(path,'avhrr-only-v2.20120101.nc'),'lon',1,52)];

%%
min_year = 1982;
max_year = 1982;
range_year = max_year - min_year +1;

%%
%calculate mean
%DATRAS
formatOut = 'yyyymmdd';
sst_DATRAS=zeros(50,16+52,31,12,1);  % initialize (lat/long/day/month/ total years) 50*68=3400
for year=min_year:max_year
    if year==1984|| year==1988|| year==1992|| year==1996|| year==2000|| year==2004|| year==2008|| year==2012|| year==2016|| year==2020; 
         leapyear=1;
    else leapyear=0;
    end 
    for mon=1:12

        if mon==2 && leapyear==0; d=28;
        elseif mon==2 && leapyear==1; d=29;
        elseif mon==4||mon==6||mon==9||mon==11; d=30;
        else d=31;
        end

        for day=1:d

            opd=fullfile('.\1_noaa_sst_nc\',sprintf('%04d',[year]),strcat(sprintf('%04d',[year]), sprintf('%02d',[mon])), strcat('/avhrr-only-v2.',datestr(datenum(year,mon,day),formatOut),'.nc'));
            sst_DATRAS(:,:,day,mon,year-1981)=[rot90(ncread(opd,'sst',[1425 559 1 1],[16 50 1 1])),rot90(ncread(opd,'sst',[1 559 1 1],[52 50 1 1]))];
        end        
    end    
end
%%
%calculate month mean
for year=1:range_year
   for mon=1:12
    m(:,:,mon,year)=mean(sst_DATRAS(:,:,:,mon,year),3);
   end
   disp(m)
end



%make list
for year=1:range_year
    disp(year)
    mkdir(strcat('.\2_noaa_sst_csv\month\',int2str([year+1981]),'\'))
    for mon=1:12
        fid=fopen(strcat('.\2_noaa_sst_csv\month\',int2str([year+1981]),'\',sprintf('%04d%02d',[year+1981], mon),'.csv'),'w');
        %fid=fopen(strcat('C:\Users\USER\Desktop\sst\DA\da',sprintf('%04d',[year+1981]),'.csv'),'w');
        fprintf(fid,'Lon,Lat,sst(degC)\n');

         for i=1:length(Lon)



             for j=1:length(Lat)

                 fprintf(fid,'%8.4f,%8.4f,%.6f\n',[Lon(i),Lat(j),m(j,i,mon,year)]);

             end
         end
         fclose(fid);
    end
end


%%
%calculate year mean
for i=1:range_year
    mm(:,:,i)=mean(m(:,:,:,i),3);
end  


%make list
mkdir('.\2_noaa_sst_csv\year\')
for year=1:range_year
   
 fid=fopen(strcat('.\2_noaa_sst_csv\year\',sprintf('%04d',[year+1981]),'.csv'),'w');
 %fid=fopen(strcat('C:\Users\USER\Desktop\sst\DA\da',sprintf('%04d',[year+1981]),'.csv'),'w');
 fprintf(fid,'Lon,Lat,sst(degC)\n');

 for i=1:length(Lon)
     for j=1:length(Lat)       
         fprintf(fid,'%8.4f,%8.4f,%.6f\n',[Lon(i),Lat(j),mm(j,i,year)]);
     end
 end
 fclose(fid);
end

