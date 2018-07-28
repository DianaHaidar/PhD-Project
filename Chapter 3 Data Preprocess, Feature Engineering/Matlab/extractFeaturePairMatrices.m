[~,~,numMatrix] = xlsread('scaledNumMatrix');
[nbr_rows,nbr_Cols] = size(numMatrix);
numMatrix = cell2mat(numMatrix);
features = {'freqLogon','freqLogonNewPc','freqConnect',...
    'freqVisitUrls','freqBrowseJobUrl','freqBrowseWikileaksUrl',...
    'freqCopyFile','fileAccessExtExe',...
    'freqSendEmail','nbrAttachments','avgSizeEmails',...
    'nbrRecip','nbrToRecip','nbrCcRecip','nbrBccRecip','nonEmpRecipFlag'};

% for f1=1:nbr_Cols
%     for f2=f1+1:nbr_Cols
%         pairMatrix=zeros(nbr_rows,2);
%         pairMatrix(:,:) = numMatrix(:,[f1 f2]);
%         
%         fpair = strcat(features{f1},features{f2});
%         outputFile = strcat(fpair, '.xlsx');
%         fprintf('\nWriting into file %s', outputFile); 
%         xlswrite(outputFile, pairMatrix);
%     end
% end

for f1=1:nbr_Cols
    f2 = randi(16);
    while f2==f1
        f2 = randi(16);
    end
    pairMatrix=zeros(nbr_rows,2);
    pairMatrix(:,:) = numMatrix(:,[f1 f2]);

    fpair = strcat(features{f1},features{f2});
    outputFile = strcat(fpair, '.xlsx');
    fprintf('\nWriting into file %s', outputFile); 
    xlswrite(strcat('featurePairs\',outputFile), pairMatrix);
end
