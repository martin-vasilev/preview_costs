function [ Bnds ] = getBnds(sentenceString)
%getBnds Finds word boundaries in the sentence
%   Martin Vasilev, 2017

global Visual;

pos= strfind(sentenceString, ' ');
% boundary location is in the beginning of the empty space before the critical
% word
for i=1:length(pos)
    Bnds(i)= Visual.sentPos(1)+ pos(i)*Visual.Pix_per_Letter - Visual.Pix_per_Letter+1;%+ Visual.Pix_per_Letter;
end

end

