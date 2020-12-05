clc;
clear;

% Coppied data from text book and  used transpose rather than re-entering 
NACA2412 = [-2.0 0 2.0 4.0 6.0 8.0 10.0 12.0 14.0
            0.05 0.25 0.44 0.64 0.85 1.08 1.26 1.43 1.56 
            0.006 0.006 0.006 0.007 0.0075 0.0092 0.0115 0.0150 0.0186 
            -0.042 -0.040 -0.038 -0.036 -0.036 -0.036 -0.034 -0.030 -0.025];
        
%NACA2412 = NACA2412';

% calculate relevent value
alpha = NACA2412(1,:);
cl = NACA2412(2,:);
cmc4 = NACA2412(4,:);

xcpc = (1/4)-(cmc4 ./ cl)

% plot
plot(alpha, xcpc, "r:",'Linewidth',2)
hold on
plot(alpha, xcpc, "k.",'MarkerSize',20)
grid on
%title('x_{cp}/c vs \alpha')
%title('MAE 136 Problem 1.6'); 
title('Center of Pressure vs. Angle of Attack'); 
xlabel('\alpha', 'FontSize', 18);
ylabel('x_{cp}/c', 'FontSize', 12);

hold off 