% Label the data
[~,~,commMatrix] = xlsread('commMatrix');
[nbr_rows,~] = size(commMatrix);
commMatrixLabel=cell(nbr_rows,1);
list = dir('r5.2-*-*.xlsx');
list_length = length(list);

count=0;
t_start_1 = tic;
for f = 1: list_length 
    [~,~,scenario] = xlsread(list(f).name);
    malRecords = scenario(:,3);
    elapsed_time_1 = toc(t_start_1);
    fprintf('\n%d', elapsed_time_1);
    for m=1:numel(malRecords)
        fprintf('entered\n');
        date = datenum(malRecords{m,1},'mm/dd/yyyy HH:MM:SS');
        for s=2:nbr_rows
            lowerbound = datenum(commMatrix{s,2},'mm/dd/yyyy HH:MM:SS');
            upperbound_temp = lowerbound + hours(4);
            upperbound = datenum(upperbound_temp);  
            lTrue = find( date >= lowerbound &  date < upperbound);
            if isempty(lTrue)==0
                fprintf('\nAnomalous');
                count=count+1;
                commMatrixLabel{s,1} = 'Anomalous';
                break;
            else
                fprintf(' ');
            end 
        end
    end
   
end
fprintf('\nWriting into file commMatrixLabel.xlsx...');
xlswrite('commMatrixLabel.xlsx', commMatrixLabel);
