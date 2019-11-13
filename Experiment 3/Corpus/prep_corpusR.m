
clear all;

% load in corpus:
load_corpus;


% write valid:
fid = fopen('valid.txt','wt');

for i=1:height(sent)
    if i< height(sent)
        fprintf(fid, char(sent.Sentence(i)));
        fprintf(fid, '\n');
    else
        fprintf(fid, sent.Sentence(i));
    end
end

fclose(fid);

% write orth:
fid = fopen('orth.txt','wt');

for i=1:height(sent)
    string= sent.Sentence(i);
    orth_mask= sent.PW(i);
    target= sent.Word(i);
    new_string= strrep(string, target, orth_mask);
    
    if i< height(sent)
        fprintf(fid, char(new_string));
        fprintf(fid, '\n');
    else
        fprintf(fid, char(new_string));
    end
end

fclose(fid);


% write mask:
fid = fopen('mask.txt','wt');

for i=1:height(sent)
    string= sent.Sentence(i);
    mask= sent.Mask(i);
    target= sent.Word(i);
    new_string= strrep(string, target, mask);
    
    if i< height(sent)
        fprintf(fid, char(new_string));
        fprintf(fid, '\n');
    else
        fprintf(fid, char(new_string));
    end
end

fclose(fid);

% boundary location in characters

% write mask:
fid = fopen('Bnd.txt','wt');

for i=1:height(sent)
    string= sent.Sentence(i);
    Nloc= sent.N_pos(i);
    words= strsplit(string, ' ');
    preBndW= words(1:Nloc);
    preBndSent= strjoin(preBndW, ' ');
    Bnd= strlength(preBndSent)+1; % +1 to get the first letter of target word
  
    if i< height(sent)
        fprintf(fid, num2str(Bnd));
        fprintf(fid, '\n');
    else
        fprintf(fid, num2str(Bnd));
    end
end

fclose(fid);


