% UPDATED: a session slot is per 4 hrs.
% [~,~,commProfile] = xlsread('commProfile');
[nbr_rows,~] = size(commProfile);
% [~,~,commEmail] = xlsread('commEmail');
% [~,~,commHttp] = xlsread('commHttp');
% [~,~,commFile] = xlsread('commFile');

% Find session slots: per 4 hrs 
datetime = commProfile(2:end,4);
dn = datenum(datetime, 'mm/dd/yyyy HH:MM:SS');   % Convert to date number
[dns,sidx] = sort(dn);  
minDT = min(dns);
maxDT = max(dns);
     
nbr_sessionslots = 0;
% count nbr of session slots
for s=minDT:hours(4):maxDT
    nbr_sessionslots = nbr_sessionslots + 1;
end

lowerbound = minDT;
commMatrixSlots = cell(nbr_sessionslots, 2);
commMatrixF1 = cell(nbr_sessionslots, 1);
commMatrixF2 = cell(nbr_sessionslots, 1);
commMatrixF3 = cell(nbr_sessionslots, 1);
commMatrixF4 = cell(nbr_sessionslots, 1);
commMatrixF5 = cell(nbr_sessionslots, 1);
commMatrixF6 = cell(nbr_sessionslots, 1);
commMatrixF7 = cell(nbr_sessionslots, 1);
commMatrixF8 = cell(nbr_sessionslots, 1);
commMatrixF9 = cell(nbr_sessionslots, 1);
commMatrixF10 = cell(nbr_sessionslots, 1);
commMatrixF11 = cell(nbr_sessionslots, 1);
commMatrixF12 = cell(nbr_sessionslots, 1);
commMatrixF13 = cell(nbr_sessionslots, 1);
commMatrixF14 = cell(nbr_sessionslots, 1);
commMatrixF15 = cell(nbr_sessionslots, 1);
commMatrixF16 = cell(nbr_sessionslots, 1);
commMatrixF17 = cell(nbr_sessionslots, 1);
commMatrixF18 = cell(nbr_sessionslots, 1);

% fill session slot id
parfor s=1:nbr_sessionslots
    commMatrixSlots{s,1} = s;
end

% fill session slot lowerbound
parfor s=1:nbr_sessionslots
    commMatrixSlots{s,2} = datestr(lowerbound + hours(4) * (s-1), 'mm/dd/yyyy HH:MM:SS');    % lowerbound datetime
end
fprintf('slots generated\n');

count =0;
tt=0;
t_start = tic;
% Build extracted commMatrix from commProfile
%logon records
 logonActTrue = ismember(commProfile(:,5), 'logon'); %where user logon in commProfile 1 0 same size
 logonActProfile = commProfile(logonActTrue,4); %get dates where logon satisfied size truncated/phase1
 logonDate = datenum(logonActProfile,'mm/dd/yyyy HH:MM:SS');
 
 % connect
 connectActTrue = ismember(commProfile(:,5), 'connect');
 connectActProfile = commProfile(connectActTrue,4);
 connectDate = datenum(connectActProfile,'mm/dd/yyyy HH:MM:SS');

 % visitUrl
 VisitUrlActTrue = ismember(commProfile(:,5), 'visitUrl'); %where visitUrl in commProfile 1 0 same size
 VisitUrlActProfile = commProfile(VisitUrlActTrue,4); %get dates where visitUrl satisfied size truncated/phase1
 VisitUrlDate = datenum(VisitUrlActProfile,'mm/dd/yyyy HH:MM:SS');
 
 % copyFile
copyFileActTrue = ismember(commProfile(:,5), 'copyFile');
copyFileActProfile = commProfile(copyFileActTrue,4);
copyFileDate = datenum(copyFileActProfile,'mm/dd/yyyy HH:MM:SS');

% sendEmail
sendEmailActTrue = ismember(commProfile(:,5), 'sendEmail');
sendEmailActProfile = commProfile(sendEmailActTrue,4);
sendEmailDate = datenum(sendEmailActProfile,'mm/dd/yyyy HH:MM:SS');
    
%% 
