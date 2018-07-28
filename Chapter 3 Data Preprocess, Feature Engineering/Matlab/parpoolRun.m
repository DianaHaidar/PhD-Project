parfor s=1:nbr_sessionslots
    %nbr_sessionslots
    %session slot bounds
    fprintf('\nslot:%d',s);
    lowerbound = datenum(commMatrixSlots{s,2},'mm/dd/yyyy HH:MM:SS');
    upperbound_temp = lowerbound + hours(4);
    upperbound = datenum(upperbound_temp);  
    
%   freqLogon
     lTrue = find(logonDate >= lowerbound & logonDate < upperbound);
%      fprintf('%d', numel(lTrue));
     datesTrue = logonDate(lTrue); %get dates where logon satisified size truncated/phase2
     freqLogon = numel(lTrue);
     commMatrixF1{s,1} = freqLogon;

%   logon from new pc
   
    logonNewPc = 0; % flag={0,1}
    pcs = commProfile(logonActTrue,3);  %get pcs where logon statisfied size truncted/phase1
    defaultPc = pcs{1,1};
    vTrue = pcs(lTrue);  %get pcs where logon satisified size truncated/phase2
    count = count + numel(vTrue);
    freqLogonNewPc=0;

    for i=1:numel(vTrue)
        if strcmp(vTrue{i}, defaultPc) == 0
            logonNewPc = 1;
            freqLogonNewPc=freqLogonNewPc+1;
        end
    end  
    commMatrixF2{s,1} = logonNewPc;

% freqLogonAfterHrs    
    mydatestr = logonActProfile; 
    dateProfile = mydatestr(lTrue);  
    
    time = zeros(numel(dateProfile),1);
    for i=1:numel(dateProfile)
        t = strsplit(dateProfile{i},' ');
        time(i) = datenum(t{2}, 'HH:MM:SS');
    end
    startWorkHrs = datenum('07:00:00', 'HH:MM:SS');
    endWorkHrs = datenum('20:00:00', 'HH:MM:SS');
    tTrue = find(time < startWorkHrs & time > endWorkHrs);
    logonAfterHrs = tTrue;   % returns 1 instead of 0 and 0 elsewhere % get rows in afterHrs
    freqLogonAfterHrs = numel(logonAfterHrs);
    commMatrixF3{s,1} = freqLogonAfterHrs;
    
% freqConnect
    lTrue = find(connectDate >= lowerbound & connectDate < upperbound);
    datesTrue = connectDate(lTrue);
    freqConnect = numel(lTrue);
    commMatrixF4{s,1} = freqConnect;
    
% freqConnectAfterHrs
    mydatestr = connectActProfile; 
    dateProfile = mydatestr(lTrue);  
    
    time = zeros(numel(dateProfile),1);
    for i=1:numel(dateProfile)
        t = strsplit(dateProfile{i},' ');
        time(i) = datenum(t{2}, 'HH:MM:SS');
    end
    tTrue = find(time < startWorkHrs & time > endWorkHrs);
    connectAfterHrs = tTrue;   % returns 1 instead of 0 and 0 elsewhere % get rows in afterHrs
    freqConnectAfterHrs = numel(connectAfterHrs);
    commMatrixF5{s,1} = freqConnectAfterHrs;
                                                         
%     freqVisitUrls 
    lTrue = find(VisitUrlDate >= lowerbound & VisitUrlDate < upperbound);
    freqVisitUrls = numel(lTrue);
    commMatrixF6{s,1} = freqVisitUrls;

%freqBrowseJobUrl
    urls = commHttp(2:end,5);
    vTrue = urls(lTrue); %urls where visitUrl satisfied date satisfied size truncated/phase2
     
    freqBrowseJobUrl=0;
    for i=1:numel(vTrue)
        if isempty(strfind(vTrue{i},'job'))==0  %may add other job websites
            freqBrowseJobUrl=freqBrowseJobUrl+1;
        end
    end
    commMatrixF7{s,1} = freqBrowseJobUrl;
     
%freqBrowseWikileaksUrl
    freqBrowseWikileaksUrl=0;
    for i=1:numel(vTrue)
        if isempty(strfind(vTrue{i},'wikileaks'))==0  %may add other job websites
            freqBrowseWikileaksUrl=freqBrowseWikileaksUrl+1;
        end
    end
    commMatrixF8{s,1} = freqBrowseWikileaksUrl;
    
%     freqCopyFiles 
    lTrue = find(copyFileDate >= lowerbound & copyFileDate < upperbound);
    freqCopyFiles = numel(lTrue);
    commMatrixF9{s,1} = freqCopyFiles;
   
%fileAccessExtExe flag={0,1}
    fileExt = commFile(2:end,5);
    vTrue = fileExt(lTrue); %file extensions where copyFile satisfied date satisfied size truncated/phase2
    fileAccessExtExe=0;
    
    for i=1:numel(vTrue)
        if strcmp(vTrue{i},'.exe')==1
             fileAccessExtExe=1;
        end
    end
    commMatrixF10{s,1} = fileAccessExtExe;
   
%     freqSendEmails 
   
    lTrue = find(sendEmailDate >= lowerbound & sendEmailDate < upperbound);
    freqSendEmails = numel(lTrue);
    commMatrixF11{s,1} = freqSendEmails;
    
%     nbrAttachments
    eDate = datenum(commEmail(2:end,4),'mm/dd/yyyy HH:MM:SS');
    lTrue = find(eDate >= lowerbound & eDate < upperbound);
    attachments = cell2mat(commEmail(2:end,10));
    vTrue = attachments(lTrue);
    nbrAttachments = sum(vTrue);
    commMatrixF12{s,1} = nbrAttachments;
       
%     avgSizeEmails
    sizeEmails = cell2mat(commEmail(2:end,9));
    vTrue = sizeEmails(lTrue);
    avgSizeEmails = round(sum(vTrue)/numel(vTrue));
    commMatrixF13{s,1} = avgSizeEmails;

%    nbrRecip nbrToRecip nbrCcRecip nbrBccRecip
    nonEmpRecipFlag=0;     

    nbrRecip = 0;
    nbrToRecip = 0;
    toRecip = commEmail(2:end,6);
    vTrue = toRecip(lTrue); %check
    for k=1:numel(vTrue)
        if isfloat(vTrue{k}) == 0
            temp = strsplit(vTrue{k}, ';');
            nbrToRecip = nbrToRecip + numel(temp);
            nbrRecip = nbrRecip + numel(temp);
            
            for i=1:numel(temp)
                email=strsplit(temp{i}, '@');
                ext=email{2};
                if strcmp(ext, 'dtaa.com') == 0
                    if nonEmpRecipFlag==0
                        nonEmpRecipFlag=1;
                    end
                end
            end
        end
    end
    commMatrixF14{s,1} = nbrToRecip;
    
    nbrCcRecip = 0;
    ccRecip = commEmail(2:end,7);
    vTrue = ccRecip(lTrue);
    for k=1:numel(vTrue)
        if isfloat(vTrue{k}) == 0
            temp = strsplit(vTrue{k}, ';');
            nbrCcRecip = nbrCcRecip + numel(temp);
            nbrRecip = nbrRecip + numel(temp);
            
            for i=1:numel(temp)
                email=strsplit(temp{i}, '@');
                ext=email{2};
                if strcmp(ext, 'dtaa.com') == 0
                    if nonEmpRecipFlag==0
                        nonEmpRecipFlag=1;
                    end
                end
            end
        end
    end
    commMatrixF15{s,1} = nbrCcRecip;
    
    nbrBccRecip = 0;
    bccRecip = commEmail(2:end,8);
    vTrue = bccRecip(lTrue);
    for k=1:numel(vTrue)
        if isfloat(vTrue{k}) == 0
            temp = strsplit(vTrue{k}, ';');
            nbrBccRecip = nbrBccRecip + numel(temp);
            nbrRecip = nbrRecip + numel(temp);
            
            for i=1:numel(temp)
                email=strsplit(temp{i}, '@');
                ext=email{2};
                if strcmp(ext, 'dtaa.com') == 0
                    if nonEmpRecipFlag==0
                        nonEmpRecipFlag=1;
                    end
                end
            end
        end
    end
    commMatrixF16{s,1} = nbrBccRecip;
    commMatrixF17{s,1} = nbrRecip;
    
% nonEmpRecipFlag 
    commMatrixF18{s,1} = nonEmpRecipFlag;
end