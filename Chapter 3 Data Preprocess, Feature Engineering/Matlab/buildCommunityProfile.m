function [commProfile, commHttp, commFile, commEmail] = buildCommunityProfile(role)
% this version is modified according to r5.1-4 data
%

% [commProfile, commHttp, commFile, commEmail] = buildCommunityProfile('ITAdmin')
% pre-step: convert .csv to .xlsx

% Goal: build a community profile
% docs: logon, device, http, file, email

%% Section 1: find users belonging to a community i.e. having the same role
[~,~,data] = xlsread('2009-12');
lTrue = ismember(data(:,4), role); % ismember returns 1 if true, 0 if false while find returns indices of satisfied answers
community = data(lTrue, 2);

%% Section 1: build file commProfile
t_start_1 = tic;
fprintf('\n%d', t_start_1);
docs = {'logon', 'device', 'http1','http2','http3', 'file', 'email'};
nbr_docs = length(docs);
nbr_rows = zeros(1,nbr_docs);
for f = 1: nbr_docs 
    [~,~,data] = xlsread(docs{f});
    lTrue = ismember(data(:,3), community);
    commRows = data(lTrue);
    [nbr_rows(f),~] = size(commRows);
end
elapsed_time_1 = toc(t_start_1);
fprintf('\n%d', elapsed_time_1);

commProfile_nbr_rows = sum(nbr_rows) + 1;
commProfile_nbr_cols = 5;
commProfile = cell(commProfile_nbr_rows,commProfile_nbr_cols);
commProfile(1,:)  = {'record_id', 'user_id', 'pc', 'datetime', 'activity_id'};
record_id = 2;

commHttp_nbr_cols = 5;
commHttp = cell(nbr_rows(3) +nbr_rows(4) +nbr_rows(5) +1,commHttp_nbr_cols);
commHttp(1,:) = {'seq_id', 'record_id', 'user_id', 'datetime', 'url'};
commHttp_seq_id = 2;

commFile_nbr_cols = 5;
commFile = cell(nbr_rows(6) + 1,commFile_nbr_cols);
commFile(1,:) = {'seq_id', 'record_id', 'user_id', 'datetime', 'file_extension'};
commFile_seq_id = 2;

commEmail_nbr_cols = 10;
commEmail = cell(nbr_rows(7) + 1,commEmail_nbr_cols);
commEmail(1,:) = {'seq_id', 'record_id', 'user_id', 'datetime', 'from', 'to', 'cc', 'bcc', 'size', 'attachment_count'};
commEmail_seq_id = 2;

t_start_2 = tic;
for f = 1: nbr_docs
    
    [~,~,data] = xlsread(docs{f});
    lTrue = ismember(data(:,3), community);
    data = data(lTrue,:);
    
    for k = 1: nbr_rows(f)
        %
        commProfile{record_id,1} = record_id;
        commProfile{record_id,2} = data{k,3};    % user_id
        commProfile{record_id,3} = data{k,4};    % pc
        datetime = data{k,2};
%         temp = strsplit(data{k,2},' ');
%         date = temp{1};
%         time = temp{2};
        commProfile{record_id,4} = datetime;    % date

         if strcmp(docs{f}, 'logon') == 1 ...
                || strcmp(docs{f}, 'device') == 1
            commProfile{record_id,5} = lower(data{k,5});

        elseif contains(docs{f}, 'http') == 1 
			commProfile{record_id,5} = 'visitUrl';      % activity_value
            %
            commHttp{commHttp_seq_id,1} = commHttp_seq_id;           % seq_id
            commHttp{commHttp_seq_id,2} = record_id;   % record_id
            [http] = strsplit(data{k,5}, '/');
            commHttp{commHttp_seq_id,3} = data{k,3};              % user_id
            commHttp{commHttp_seq_id,4} = datetime;              % date
            commHttp{commHttp_seq_id,5} = strcat('http://', http{2});   % url

            commHttp_seq_id = commHttp_seq_id + 1;
            %
        elseif strcmp(docs{f}, 'file') == 1
            commProfile{record_id,5} = 'copyFile';
            %
            commFile{commFile_seq_id,1} = commFile_seq_id;               % seq_id
            commFile{commFile_seq_id,2} = record_id;       % record_id
            [file] = strsplit(data{k,5}, '.');
            commFile{commFile_seq_id,3} = data{k,3};                  % user_id
            commFile{commFile_seq_id,4} = datetime;                  % date
            commFile{commFile_seq_id,5} = strcat('.', file{2});       % file_extension

            commFile_seq_id = commFile_seq_id + 1;
            %
        elseif contains(docs{f}, 'email') == 1 
            commProfile{record_id,5} = 'sendEmail';
            %
            commEmail{commEmail_seq_id,1} = commEmail_seq_id;             % seq_id
            commEmail{commEmail_seq_id,2} = record_id;      % record_id
            commEmail{commEmail_seq_id,3} = data{k,3};                 % user_id
            commEmail{commEmail_seq_id,4} = datetime;                 % date
            commEmail{commEmail_seq_id,5} = data{k,8};                 % from
            commEmail{commEmail_seq_id,6} = data{k,5};                 % to
            commEmail{commEmail_seq_id,7} = data{k,6};                 % cc
            commEmail{commEmail_seq_id,8} = data{k,7};                 % bcc
            commEmail{commEmail_seq_id,9} = data{k,10};                 % size
            if isfloat(data{k,11}) == 0
                [attachs] = strsplit(data{k,11}, ';');
                attach_count = numel(attachs);
            else
                attach_count = 0;
            end
            commEmail{commEmail_seq_id,10} = attach_count;               % attachment_count

            commEmail_seq_id = commEmail_seq_id + 1;
            %
        end

        record_id = record_id + 1;
        %
    end
    
end
elapsed_time_2 = toc(t_start_2);
fprintf('\n%d', elapsed_time_2);

t_start_3 = tic;
% fprintf('\nWriting into file commProfile.xlsx...');
% xlswrite('commProfile.xlsx', commProfile);
% fprintf('\nWriting into file commHttp.xlsx...');
% xlswrite('commHttp.xlsx', commHttp);
% fprintf('\nWriting into file commFile.xlsx...');
% xlswrite('commFile.xlsx', commFile);
% fprintf('\nWriting into file commEmail.xlsx...');
% xlswrite('commEmail.xlsx', commEmail);
elapsed_time_3 = toc(t_start_3);
fprintf('\n%d', elapsed_time_3);
end