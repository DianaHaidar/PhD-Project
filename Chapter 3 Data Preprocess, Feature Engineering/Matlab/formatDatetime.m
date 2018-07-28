datetime = commProfile(2:end,4);
for d=1:numel(datetime)
    try 
        dn = datenum(datetime{d}, 'mm/dd/yyyy HH:MM:SS'); 
    catch
        fprintf('\n%d',d);
    end
end
formatted = datetime;

for d=1:numel(datetime)
%        if regexp(datetime{d},'\d{2}\/\d{2}\/\d{4} \d{2}:\d{2}:\d{2}$')
% %           fprintf('\n%s',datetime{d});
       if regexp(datetime{d},'\d{2}\/\d{2}\/\d{4}$')
          fprintf('\n%s',datetime{d});
          formatted{d}= strcat(datetime{d},' 00:00:01');
       elseif regexp(datetime{d},'\d{2}\/\d{2}\/\d{4} 00:00:00$')
          fprintf('\n%s',datetime{d});
          date = strsplit(datetime{d},' ');
          formatted{d}= strcat(date,' 00:00:01');
       elseif isempty(datetime{d})
          formatted{d}= formatted{d-1};
       end
end

datetime = formatted;
for d=1:numel(datetime)
    try 
        dn = datenum(datetime{d}, 'mm/dd/yyyy HH:MM:SS'); 
    catch
        fprintf('\n%d',d);
    end
end
commProfile(2:end,4)=datetime;
