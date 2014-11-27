%pings = importdata('Pings.txt');
pings = importdata('PingData00.txt');

pings = mean(pings);

days = [];
for day = 1:35
    tmp = zeros(1,24);
    tmp(tmp == 0) = day;
    days = horzcat(days,tmp);
end

out = grpstats(pings, days, @mean)

weekDays = [];
% 1 .. 35 days
for (weekDay = 1:length(pings)/24)
    weekDays = horzcat(weekDays, mod(weekDay, 7));
end
% per weekday
weekDaysOut = grpstats(out, weekDays, @mean)
bar(weekDaysOut)
ylabel('availability');
xlabel('days');

% per day
%bar(out)
%ylabel('availability');
%xlabel('days');