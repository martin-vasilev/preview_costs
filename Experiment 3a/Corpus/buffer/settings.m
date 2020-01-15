global Visual const;

%% Visual settings
Visual.resX= 1024; % Screen resolution, X-axis; P03i (CRT)
Visual.resY= 768; % Screen resolution, Y-axis; P03i (CRT)
Visual.frameRate= 150; % frame rate of monitor (saved to edf file)
Visual.offsetX= 50; % X offset of the text
Visual.offsetY= Visual.resY/2; % Y offset of the text
Visual.sentPos= [Visual.offsetX Visual.offsetY]; % sentence position
Visual.FGC= [0 0 0]; % stimuli colour
Visual.BGC= [255 255 255]; % background colour
Visual.Pix_per_Letter= 11; % letter width in pixels
Visual.FontSize= 17; % # Courier New: 18pt: 14ppl; 16pt: 13ppl; 14pt: 11ppl

Visual.Font= 'Courier'; %'Courier New';
Visual.InstrTextSize= 24; % unused
Visual.GazeBoxSize= 40; % gaze box size in pixels
Visual.GazeBoxColor= [0 0 0]; % colour of the gaze-box
Visual.gazeBoxDur= 100; % how many ms the eye needs to stay on the gaze box before triggering it
Visual.gazeBoxDisplayTime= 7; % how many seconds to wait to trigger the gaze box

%% Experiment settings:
const.breakTime= 3*60; % break time in the experiment (in seconds)
const.TrialTimeout= 60; % automatically terminates trial after x seconds
const.Maxtrials= 138; % number of experimental trials 

%% !!! DANGER ZONE !!!:
const.checkPPL= false;  % if true, draws a rectangle around sentence to make sure letter width is correct
const.seeBoundary= false; % draw boundary location on screen (testing only!)
const.seeEye= false; % draw gaze position on screen (testing only!)
const.expName = 'E2DEG'; % used for saving edf data (keep at <= 5 letters)
const.caltype= 'H3'; % calibration type; use 'HV9' for 9-point grid and 'H3' for three-point calibration
const.saccvelthresh = 35; % degrees per second, saccade velocity threshold; don't change
const.saccaccthresh = 9500; % degrees per second, saccade acceleration threshold; don't change
