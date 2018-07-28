[~,~,commMatrix] = xlsread('commMatrix');
numMatrix = cell2mat(commMatrix(2:end,3:end));
[~,nbr_Cols] = size(numMatrix);
for c=1:nbr_Cols
    minCol = min(numMatrix(:,c));
    maxCol = max(numMatrix(:,c));
    numMatrix(:,c) =(numMatrix(:,c)-minCol)/(maxCol-minCol);
end
fprintf('\nWriting into file scalednumMatrix.xlsx...');
xlswrite('scaledNumMatrix.xlsx', numMatrix);