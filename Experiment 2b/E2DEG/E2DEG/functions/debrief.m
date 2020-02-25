function [] = debrief(item)
% Displays Item on the screen to check formatting, etc.
%   Press a key to exit window
global Visual Monitor;

if nargin <1
    item= 1;
end

settings; % load settings
load('sent.mat'); % items

% get item from sentence frame:
%whichRow= find(sent.item== item & sent.cond== cond, 1);
%sentenceString= char(sent.stimuli(whichRow));
%sentenceString= strjoin(strsplit(sentenceString, '"'));

% open screen:
Screen('Preference', 'SkipSyncTests', 1); 
oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
	
% Find out how many screens and use largest screen number.
whichScreen = max(Screen('Screens'));

%% Setup window:
Monitor.window = Screen('OpenWindow', whichScreen);
Screen(Monitor.window, 'TextSize', Visual.FontSize);
Screen(Monitor.window, 'TextFont', Visual.Font);
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

for i=1:4
   Monitor.buffer(i)= Screen(Monitor.window, 'OpenOffscreenWindow');
   Screen(Monitor.buffer(i), 'TextSize', Visual.FontSize);
   Screen(Monitor.buffer(i), 'TextFont', Visual.Font);
   Screen(Monitor.buffer(i), 'TextStyle', 1); % normal
end

conditions= [4,2,3];

%% Get stimuli:
for i=1:length(conditions)
    HideCursor;
    boundaryCrossed=false;
    trialEnd= false;
    cond= conditions(i);

    sentenceString= char(sent.Sentence(item));

    % load post-boundary image (always valid with 0% deg):
    if item> const.Maxtrials % practice
        postBndImg_filename= ['img\' num2str(item) '_pract_0.bmp'];
        postBndImg= read8bit(postBndImg_filename); % read in image 

    else % experimental
        postBndImg_filename= ['img\' num2str(item) '_valid_0.bmp'];
        postBndImg= read8bit(postBndImg_filename); % read in image
    end

    % Cond 1:    valid preview, 0% degradation
    % Cond 2:    orth preview, 0% degradation
    % Cond 3:    mask preview, 0% degradation
    % Cond 4:    valid preview, 20% degradation
    % Cond 5:    orth preview, 20% degradation
    % Cond 6:    mask preview, 20% degradation

    % load pre-boundary image:
    if cond < 4
        deg= '0';
    else
        deg= '20';
    end

    if cond==1 || cond==4
        prev= 'valid';
    end

    if cond==2 || cond==5
        prev= 'orth';
    end

    if cond==3 || cond==6
        prev= 'mask';
    end


    if item> const.Maxtrials % practice
        preBndImg_filename= ['img\' num2str(item) '_pract_' deg '.bmp'];
        preBndImg= read8bit(preBndImg_filename); % read in image 
    else % experimental
        preBndImg_filename= ['img\' num2str(item) '_' prev '_' deg '.bmp'];
        preBndImg= read8bit(preBndImg_filename); % read in image
    end

    % get Boundary location:
    Bnds= getBnds(sentenceString); % all word boundaries
    boundary= Bnds(sent.N_pos(item)); % target word boundary (after word N)

    Screen('FillRect', Monitor.buffer(2), Visual.BGC);
    Screen('FillRect', Monitor.buffer(4), Visual.BGC);

    % put pre-boundary image on the screen:
    Screen('PutImage', Monitor.buffer(2), preBndImg);

    % put post-boundary image on the screen:
    Screen('PutImage', Monitor.buffer(4), postBndImg);
    
    % add boundary for easy noticing:
    Screen('DrawLine', Monitor.buffer(2), [176,9,9], boundary, 300, boundary+1, 500);
    Screen('DrawLine', Monitor.buffer(4), [176,9,9], boundary, 300, boundary+1, 500);

    %% "Gaze"-contingent part:

    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window); % present sentence
    Screen('CopyWindow', Monitor.buffer(4), Monitor.window);

    SetMouse(50, Visual.resY/2);
    ShowCursor(0);

    while ~trialEnd
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); % wait for mouse press (left button)

        % boundary manipulation:
        if x >= boundary && ~ boundaryCrossed
            Screen('Flip', Monitor.window); % refresh screen
            boundaryCrossed= true;
        end

    end

    
end



% print sentence on screen:
%DrawFormattedText(Monitor.window, sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC, ...
%                          [], [], [], Visual.TextSpacing*1.95);
% Screen('Flip', Monitor.window); % present sentence

% wait for keypress to terminate:

KbWait();
Screen('CloseAll');


end

