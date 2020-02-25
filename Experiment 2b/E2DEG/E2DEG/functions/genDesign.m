function design = genDesign()

% Generate a design matrix for the current subject
% Victoria Adedeji & Martin Vasilev, 2019

global const;

% Cond 1:    valid preview, 0% degradation
% Cond 2:    orth preview, 0% degradation
% Cond 3:    mask preview, 0% degradation
% Cond 4:    valid preview, 20% degradation
% Cond 5:    orth preview, 20% degradation
% Cond 6:    mask preview, 20% degradation
 
design_same = load('design_same.dat'); % load the data (from xLatinSquare):
D= size(design_same);

if const.ID>D(2)-1 % for testing
    cps = design_same(:,[1, 2]);
    warning('Using a test subject number!');
    
else % real subject
   cps = design_same(:,[1, const.ID+1]); % +1 because the first column is item number(%cps is conditions per subject) 
end


%Randomise the items:
%cps_r= cps(randperm(length(cps)),:);
cps_r = cps(randperm(size(cps, 1)), :);

%Practice items:
c1 = (139:144)'; % column 1 is item number

if mod(const.ID, 2)==1
    c2 = [1,1,1,4,4,4]'; % column 2 is condition 
else
    c2 = [4,4,4,1,1,1]'; % column 2 is condition 
end

pr = horzcat(c1,c2); % 2- column matrix 

%Randomise practice items as above:
prr = pr(randperm (length(pr)),:);

% % combine practice & experimental items:
design = [prr; cps_r];

% save design matrix for our records:
savefile= ['design/sub_matrix/s_' num2str(const.ID) '.mat'];
save(savefile, 'design');

end
