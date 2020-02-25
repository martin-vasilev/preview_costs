cd('D:\R\preview_costs\Experiment 3\Corpus');

files= dir('img'); 
files(1:2)= []; %files(641)= [];
ResX= 1024;
ResY= 768;

for i =1:length(files)
  %a= I(1,1);
  [I, map] = imread([cd '\img\' files(i).name]);
  I= [I(:,1), I]; % make it start at 50-pixels
  add= repmat(I(:,1),1, ResX-length(I));
  I(:, 1025)= [];
  I= [I, add];
  I1(1:364,1:ResX)= I(1,1); 
  I2(1:364,1:ResX)= I(1,1); 
  IF= [I1; I; I2];
  %IF=I;
  %[r, c]= find(IF==IF(1,1));
  %IF(r, c)=255; 
  
  %output= [cd '\resized\' '' files(i).name]; 
  output= [cd '\resized\' '' files(i).name(1:end-3) 'bmp']; 
  
  imwrite (IF, map, output);
  i
end

%[I, map] = imread('corpus/bmp/1_MASK_20.bmp');

%I1(1:364,1:1024)= 229; 
%I2(1:364,1:1024)= 229; 
%IF= [I1; I; I2];
 
%imwrite (IF, map, 'test.bmp');