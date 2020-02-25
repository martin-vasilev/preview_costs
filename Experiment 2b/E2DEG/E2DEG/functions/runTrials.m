% Presentation of Experimental trials
% Martin Vasilev, 2017

global const Visual sent Monitor el; 

% Blackbox toolkit testing:
%s= serial('COM11');
%set(s, 'BaudRate', 115200, 'DataBits', 8, 'StopBits', 1, 'Parity', 'none')
%fopen(s);
%fprintf(s, 'RR');
%fprintf(s,'FF');

%const.ntrials=1; % TEMPORARY!!! Use only for testing

HideCursor; % hide the mouse cursor

% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);

% Trial presentation loop:
for i=1:const.ntrials
    
    % BREAK
    if i== round(const.ntrials/2)
        MakeBreak(const.breakTime);
    end
    
    %calResult = Eyelink('CalResult');
    
%%  stimuli set-up:
    boundaryCrossed=false;
    trialEnd= false; 
    
	item= design(i,1); % item is 1st column
    cond= design(i,2); % condition is 2nd column
    
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
    
       
    % drift check:
    EyelinkDoDriftCorrection(el);
    
    %% Eyelink & Screen trial set-up:
	stimuliOn= false;
    
    while ~stimuliOn
        if item> const.Maxtrials % if practice
            Eyelink('Message', ['TRIALID ' 'P' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
            Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'P' num2str(cond) 'I' num2str(item) 'D0']]);
        else
			Eyelink('Message', ['TRIALID ' 'E' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
			Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'E' num2str(cond) 'I' num2str(item) 'D0']]); 
        end
        
        % print boundary location to edf file:
        Eyelink('Message', ['BOUNDARY ' num2str(boundary)]);
        
        % Print image filenames to edf:
        Eyelink('Message', ['PRE-BOUNDARY IMG ' preBndImg_filename]);
        Eyelink('Message', ['POST-BOUNDARY IMG ' postBndImg_filename]);

        % print text stimuli to edf:
        stim2edf(sentenceString); % Single line
        %stim2edfML(sentenceString); % Multi-line
        
        % prepare Screens:
        % sentence presentation:
        Screen('FillRect', Monitor.buffer(2), Visual.BGC);
        Screen('FillRect', Monitor.buffer(4), Visual.BGC);
        
        % put pre-boundary image on the screen:
        Screen('PutImage', Monitor.buffer(2), preBndImg);
        
        % put post-boundary image on the screen:
        Screen('PutImage', Monitor.buffer(4), postBndImg);
        
        % see boundary (for testing)?
        if const.seeBoundary
            Screen('DrawLine', Monitor.buffer(2), Visual.FGC, boundary, 300, boundary, 500);
            Screen('DrawLine', Monitor.buffer(4), Visual.FGC, boundary, 300, boundary, 500);
        end
        
        
        % Use this for printing text as a string:
        %Screen('DrawText', Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
        %DrawFormattedText(Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC, ...
        %                  [], [], [], Visual.TextSpacing*1.95);
        
        if const.checkPPL
            MLcheck= strfind(sentenceString, '\n');
            
            if ~isempty(MLcheck)
                sentenceString= strrep(sentenceString, '\n', '@');
                sentenceString= strsplit(sentenceString, '@');
                sentenceString= char(sentenceString{1});
            end
            
			lngth= length(sentenceString)*Visual.Pix_per_Letter;
            Screen('FrameRect', Monitor.buffer(2), Visual.FGC, ...
                [Visual.offsetX Visual.offsetY- Visual.GazeBoxSize/2 ...
                Visual.offsetX+lngth Visual.offsetY+ Visual.GazeBoxSize]);
            
            Screen('FrameRect', Monitor.buffer(4), Visual.FGC, ...
                [Visual.offsetX Visual.offsetY- Visual.GazeBoxSize/2 ...
                Visual.offsetX+lngth Visual.offsetY+ Visual.GazeBoxSize]);
        end
        
        % Print stimuli to Eyelink monitor:
        % draw gaze box on tracker monitor:
       sendScreenshot(Monitor.buffer(2), 'disp.bmp');
        
        %% Present Gaze-box:
        stimuliOn= gazeBox(stimuliOn);
        
    end
    
    %% Present text stimuli:
    Eyelink('Message', 'GAZE TARGET OFF');
    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window); % present sentence
    Eyelink('Message', 'DISPLAY ON');
    Eyelink('Message', 'SYNCTIME');

	trialStart= GetSecs;
    
    % copy post-boundary screen in preparation for display change:
    Screen('CopyWindow', Monitor.buffer(4), Monitor.window);
    
    while ~trialEnd
        trialTime= GetSecs- trialStart;
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); % wait for mouse press (left button)
        
        % use this for gaze-contingent manipulations:
        evt= Eyelink('NewestFloatSample');
        xpos = max(evt.gx);

        
        % boundary manipulation:
        if xpos >= boundary && ~ boundaryCrossed
            Eyelink('Message', 'DISPLAY CHANGE STARTED');
            Screen('Flip', Monitor.window); % refresh screen
            Eyelink('Message', 'DISPLAY CHANGE COMPLETED');
            boundaryCrossed= true;
        end
        
        if const.seeEye % FOR TESTING ONLY
            Screen('FillRect', Monitor.window, Visual.BGC);
            
            if ~boundaryCrossed
                Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
            else
                Screen('CopyWindow', Monitor.buffer(4), Monitor.window);
            end

            Screen('DrawDots', Monitor.window, [xpos, Visual.resY/2], 10, Visual.FGC, [],2);
            Screen('Flip', Monitor.window);
        end
        
        % end trial automatically if no response by participant
        if trialTime> const.TrialTimeout 
             trialEnd= true;
 			 Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
             Screen('Flip', Monitor.window);
        end
        
    end	
	
	% end of trial messages:
    Eyelink('Message', 'ENDBUTTON 5');
    Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
    Screen('Flip', Monitor.window);
    Eyelink('Message', 'DISPLAY OFF');
    Eyelink('Message', 'TRIAL_RESULT 5');
    Eyelink('Message', 'TRIAL OK');
    
    Eyelink('StopRecording');
    Eyelink('command', 'clear_screen 0'); % clear tracker screen
    
   
     %% Questions:
      if sent.Has_quest(item)== 1
          
          % present question:
          answer= Question(char(sent.Question(item)), sent.Answer(item), ...
              item, cond, 'YES', 'NO', false);
      end   
    

end


% end of Experiment text:
%text= 'The experiment is finished! Thank you for participating!';
%Screen(Monitor.window, 'TextSize', Visual.InstrTextSize);
%Screen('DrawText', Monitor.window, text, Visual.resX/2- (Visual.Pix_per_Letter+3)*length(text)/2, Visual.resY/2, [139, 0, 0]);
%Screen('Flip', Monitor.window);
