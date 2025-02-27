%% Import data from text file.
% Script for importing data from the following text file:
%
%    D:\R\preview_costs\Experiment 3\Corpus\Corpus2.txt
%
% To extend the code to different selected data or a different text file,
% generate a function instead of a script.

% Auto-generated by MATLAB on 2019/11/12 22:16:00

%% Initialize variables.
filename = 'D:\R\preview_costs\Experiment 3\Corpus\Corpus2.txt';
delimiter = '\t';
startRow = 2;

%% Read columns of data as text:
% For more information, see the TEXTSCAN documentation.
formatSpec = '%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%[^\n\r]';

%% Open the text file.
fileID = fopen(filename,'r');

%% Read columns of data according to the format.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');

%% Close the text file.
fclose(fileID);

%% Convert the contents of columns containing numeric text to numbers.
% Replace non-numeric text with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = mat2cell(dataArray{col}, ones(length(dataArray{col}), 1));
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[1,6,7,8,10,11,14,15,17,18,19,20]
    % Converts text in the input cell array to numbers. Replaced non-numeric
    % text with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1)
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData(row), regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if numbers.contains(',')
                thousandsRegExp = '^[-/+]*\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(numbers, thousandsRegExp, 'once'))
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric text to numbers.
            if ~invalidThousandsSeparator
                numbers = textscan(char(strrep(numbers, ',', '')), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch
            raw{row, col} = rawData{row};
        end
    end
end


%% Split data into numeric and string columns.
rawNumericColumns = raw(:, [1,6,7,8,10,11,14,15,17,18,19,20]);
rawStringColumns = string(raw(:, [2,3,4,5,9,12,13,16]));


%% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),rawNumericColumns); % Find non-numeric cells
rawNumericColumns(R) = {NaN}; % Replace non-numeric cells

%% Make sure any text containing <undefined> is properly converted to an <undefined> categorical
for catIdx = [1,6]
    idx = (rawStringColumns(:, catIdx) == "<undefined>");
    rawStringColumns(idx, catIdx) = "";
end

%% Create output variable
Corpus2 = table;
Corpus2.item = cell2mat(rawNumericColumns(:, 1));
Corpus2.Corpus = categorical(rawStringColumns(:, 1));
Corpus2.Word = rawStringColumns(:, 2);
Corpus2.PW = rawStringColumns(:, 3);
Corpus2.Mask = rawStringColumns(:, 4);
Corpus2.n_letters_trgt = cell2mat(rawNumericColumns(:, 2));
Corpus2.n_letters_orth = cell2mat(rawNumericColumns(:, 3));
Corpus2.n_letters_mask = cell2mat(rawNumericColumns(:, 4));
Corpus2.Sentence = rawStringColumns(:, 5);
Corpus2.Length_chars = cell2mat(rawNumericColumns(:, 5));
Corpus2.Length_words = cell2mat(rawNumericColumns(:, 6));
Corpus2.Fits_on_screen = categorical(rawStringColumns(:, 6));
Corpus2.Word_N = rawStringColumns(:, 7);
Corpus2.Word_N_length = cell2mat(rawNumericColumns(:, 7));
Corpus2.Word_N1_length = cell2mat(rawNumericColumns(:, 8));
Corpus2.Question = rawStringColumns(:, 8);
Corpus2.Answer = cell2mat(rawNumericColumns(:, 9));
Corpus2.Has_quest = cell2mat(rawNumericColumns(:, 10));
Corpus2.N_pos = cell2mat(rawNumericColumns(:, 11));
Corpus2.N1_pos = cell2mat(rawNumericColumns(:, 12));

%% Clear temporary variables
clearvars filename delimiter startRow formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp rawNumericColumns rawStringColumns R catIdx idx;
sent= Corpus2;
clear Corpus2;