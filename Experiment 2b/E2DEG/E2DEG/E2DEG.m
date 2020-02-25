% A replication of PD2 (visual degradation + awareness of degraded changes)
% Martin R. Vasilev, 2019

global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('D:\R\preview_costs\Experiment 3\E2DEG\E2DEG');
addpath([cd '\functions'], [cd '\corpus'], [cd '\design'], [cd '\img']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
load('sent.mat'); % items

design= genDesign(); % generate the design matrix for this subject
const.ntrials= length(design); % number of trials

%% Run Experiment:
runTrials;

%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');
Screen('CloseAll');
