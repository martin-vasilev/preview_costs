function [img] = read8bit(filename)
%read8bit read 8 bit bmp and displays with proper rgb colours

[I, map]= imread(filename); % read in image 

img= 255*ind2rgb(I, map);

end