clear all;

global Visual;

settings;

cd('D:\R\preview_costs\Experiment 3a\Corpus\buffer');

load('sent.mat');

% open screen:
Screen('Preference', 'SkipSyncTests', 1); 
oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
	
% Find out how many screens and use largest screen number.
whichScreen = max(Screen('Screens'));

%% Setup window:
Monitor.window = Screen('OpenWindow', whichScreen);
%Screen(Monitor.window, 'TextSize', Visual.FontSize);
%Screen(Monitor.window, 'TextFont', Visual.Font);
Screen('FillRect', Monitor.window, [255, 255, 255]);
Screen('Flip', Monitor.window);

for i=1:18
    
   Monitor.buffer(i)= Screen(Monitor.window, 'OpenOffscreenWindow');
%   Screen(Monitor.buffer(i), 'TextSize', Visual.FontSize);
%   Screen(Monitor.buffer(i), 'TextFont', Visual.Font);
%   Screen(Monitor.buffer(i), 'TextStyle', 1); % normal
end

%%
i= 1;

item= i; % item is 1st column
cond= 4; % condition is 2nd column

sentenceString= char(sent.Sentence(item));

postBndImg_filename= ['img\' num2str(item) '_valid_0.bmp'];
postBndImg= read8bit(postBndImg_filename); % read in image

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
    
preBndImg_filename= ['img\' num2str(item) '_' prev '_' deg '.bmp'];
preBndImg= read8bit(preBndImg_filename); % read in image

% get Boundary location:
Bnds= getBnds(sentenceString); % all word boundaries
boundary= Bnds(sent.N_pos(item)); % target word boundary (after word N)
All_boundary= [50, Bnds]; % all boundaries (+ before 1st word)
boundaryCrossed(1:length(All_boundary))= false;
curr_Bnd= 1; % first boundary to cross is always #1

%% Generate images and place them in buffer:

new= preBndImg; % start with preview sentence for all words

for i= 1:length(All_boundary)
    if i~= length(All_boundary)
        new(1:768, All_boundary(i):All_boundary(i+1), :)= postBndImg(1:768, All_boundary(i):All_boundary(i+1), :);
    else
        new(1:768, All_boundary(i):Visual.resX, :)= postBndImg(1:768, All_boundary(i):Visual.resX, :);
    end   
 Screen('PutImage', Monitor.buffer(i), new);  
end

%Screen('PutImage', Monitor.window, new);
%Screen('CopyWindow', Monitor.buffer(6), Monitor.window);
%Screen('Flip', Monitor.window); 

%% iterate screen buffers:

for i= 1:length(All_boundary)
    
    Screen('CopyWindow', Monitor.buffer(i), Monitor.window);
    Screen('Flip', Monitor.window);
    
    WaitSecs(0.33); 
end




% wait for keypress to terminate:

KbWait();
Screen('CloseAll');
   