pings = importdata('Pings.txt');
%pings = importdata('PingData00.txt');

pings = mean(pings);

days = [];
for day = 1:35
    tmp = zeros(1,24);
    tmp(tmp == 0) = day;
    days = horzcat(days,tmp);
end

out = grpstats(pings, days, @mean)
bar(out)
ylabel('availability');
xlabel('days');