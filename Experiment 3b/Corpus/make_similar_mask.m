function similar_mask = make_similar_mask(word)


ascenders = ['t' 'd' 'f' 'h' 'k' 'l' 'b', 'i'];
descenders = ['q' 'y' 'p' 'g' 'j'];
inline = ['w' 'e' 'r' 'u' 'o' 'a' 's' 'z' 'x' 'c' 'v' 'n' 'm'];
vowels = 'aeou';
inline_consonants = 'wrszcvnm';

% make dissimilar mask
vowel_count = 0;
% for i=1:length(word)  
%    if ~isempty(strfind(vowels,word(i)))
%        vowel_count = vowel_count +1;
%    end
% end

replacedword=[];
for i=1:length(word)
    newletter=word(i);
    if ~isempty(strfind(ascenders,newletter))
        replacement = ascenders;
    elseif ~isempty(strfind(descenders,newletter))
        replacement = descenders;
    elseif ~isempty(strfind(inline,newletter))
        if i > 1
            if ~isempty(strfind(vowels,word(i-1)))
                replacement = inline_consonants;
            else
                replacement = vowels;
            end
        else
            if ~isempty(strfind(vowels,word(i+1)))
                replacement = inline_consonants;
            else
                replacement = vowels;
            end
        end
    else
        error('Invalid character %s in word "%s".',newletter,word);
    end


    
    if 0
        
	else
		possibleletters=replacement;
		possibleletters=possibleletters(possibleletters~=0);
		

		while newletter==word(i)% | newletter=='i' | newletter=='x'
			newletter=possibleletters(randsample(length(possibleletters),1));
		end
    end
    replacedword=[double(replacedword) double(newletter)];   

end

similar_mask=char(replacedword);