[data_standardized, sh, sc] = standardize(data);
save('data_standardized.mat','data_standardized');
save('sh.mat','sh');
save('sc.mat','sc');

tr_labels = [labels(1:120) ; labels(151:178) ; labels(186:209)];
save('tr_labels.mat','tr_labels');
te_labels = [labels(1:30) ; labels(151:157) ; labels(186:191)];
save('te_labels.mat','te_labels');

dc1_1 = data(1:30,:);
dc1_2 = data(31:60,:);
dc1_3 = data(61:90,:);
dc1_4 = data(91:120,:);
dc1_5 = data(121:150,:);

dc2_1 = data(151:157,:);
dc2_2 = data(158:164,:);
dc2_3 = data(165:171,:);
dc2_4 = data(172:178,:);
dc2_5 = data(179:185,:);

dc3_1 = data(186:191,:);
dc3_2 = data(192:197,:);
dc3_3 = data(198:203,:);
dc3_4 = data(204:209,:);
dc3_5 = data(210:215,:);



