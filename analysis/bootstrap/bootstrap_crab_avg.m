function bootstrap_crab_sym

% Bootstrap confidence intervals on parameter estimates 
% for functional response model fitting to crab data with Adrian Stier, Jan
% 2019

X = importdata('avg_crab_trial_data_for_ML.csv');


% Define some variables
Pred = X.data(:,2);


%Pred = Pred + min(Pred(Pred>0));
%Pred = Pred + 0.1;
Dead = X.data(:,4);
Sett = X.data(:,3);

%Dens = X.data(:,4);
%Rugo = X.data(:,5);

%Dead = max(Dead,1e-1);
%Sett = max(Sett,1e-1);
%Dead = Dead + 0.1;
%Sett = Sett + 0.1;

Surv = Sett-Dead;

hmin = 1e-10;

% Integrated feeding rate = (N(0) - N(t))/P
IntF = Dead./Pred;
%IntF = IntF + 0.1;
minI = min(IntF(IntF>0));
maxI = max(IntF(~isinf(IntF)));
IntF = min(IntF,maxI);
IntF = max(IntF,minI);

Pred(Pred==0) = 1e-3;

% Generate bootstrapped datasets:
k = 10000; % number of bootstraps
n = length(IntF); % size of original dataset
Boot = randi(n,n,k);

% Pre-allocate memory
p_h2 = nan(k,3);
p_cm = nan(k,4);
p_bd = nan(k,4);
p_hv = nan(k,4);
p_rd = nan(k,4);

options = optimset('Algorithm','sqp','Display','off');

for kk = 1:k % loop over each bootstrap

step = kk
Data_tmp = [Sett(Boot(:,kk)), IntF(Boot(:,kk)), Pred(Boot(:,kk))];    
    
% H2
p0 = [0.3, 0.1, std(Dead)];
lb = [0, hmin, 0];
ub = [Inf,Inf, Inf];
p = fmincon(@(p)h2_mle(Data_tmp(:,1),Data_tmp(:,2),Data_tmp(:,3),p),p0,[],[],[],[],lb,ub,[],options); 
p_h2(kk,:) = p(:);

% CM
p0 = [0.5, 0.1, 1, std(Dead)];
lb = [0, hmin, -Inf, 0];
ub = [Inf,Inf, Inf, Inf];
p = fmincon(@(p)cm_mle(Data_tmp(:,1),Data_tmp(:,2),Data_tmp(:,3),p),p0,[],[],[],[],lb,ub,[],options); 
p_cm(kk,:) = p(:);

% BD
p0 = [0.5, 0.1, 1, std(Dead)];
lb = [0, hmin, realmin, 0];
ub = [Inf,Inf, Inf, Inf];
p = fmincon(@(p)bd_mle(Data_tmp(:,1),Data_tmp(:,2),Data_tmp(:,3),p),p0,[],[],[],[],lb,ub,[],options); 
p_bd(kk,:) = p(:);

% HV
p0 = [0.5, 0.1, 1, std(Dead)];
lb = [0, hmin, -1e2, 0];
ub = [Inf,Inf, 1e2, Inf];
p = fmincon(@(p)hv_mle(Data_tmp(:,1),Data_tmp(:,2),Data_tmp(:,3),p),p0,[],[],[],[],lb,ub,[],options); 
p_hv(kk,:) = p(:);

% RD
p0 = [0.5, 0.1, 1, std(Dead)];
lb = [0, hmin, 1, 0];
ub = [Inf,Inf, 1, Inf];
p = fmincon(@(p)hv_mle(Data_tmp(:,1),Data_tmp(:,2),Data_tmp(:,3),p),p0,[],[],[],[],lb,ub,[],options); 
p_rd(kk,:) = p(:);

end % end loop over k

save coney_bootstrap_CI p_h2 p_cm p_bd p_hv p_rd

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define functions.  
% Time-integrated functional responses

% Holling Type II
function [L,F] = h2_mle(N0,N1,P,p)
% extract the parameters within p
a = p(1);
b = p(2);
s = p(3);
% observations lasted 1 days
t = 1;
% solution to the ODE
Nt = exp( log(N0) - wrightOmega( log(N0) + log(b) + N0.*b - P.*a.*t) + N0.*b - P.*a.*t);
F = (N0-Nt)./P; % integrated feeding rate
E = (log(F(:)) - log(N1(:))); % the difference
s = std(E);
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0
%keyboard


%%%%%%%%%%%%%%%%
% Crowley-Martin
% Note that we have set P-1 terms to be P as recommended by Skalski &
% Gilliman
function [L,F] = cm_mle(N0,N1,P,p)
% extract the parameters within p
a = p(1);
b = p(2);
c = p(3);
s = p(4);
% observations lasted 1 days
t = 1;
% solution to the ODE
Nt = exp(log(N0) - wrightOmega(log(N0) + log(b) + N0.*b - (P.*a.*t)./(P.*c + 1)) + N0.*b - (P.*a.*t)./(P.*c + 1));
F = (N0-Nt)./P; % integrated feeding rate
E = (log(F(:)) - log(N1(:))); % the difference
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0



%%%%%%%%%%%%%%%%
% Beddington-DeAngelis
% Note that we have set P-1 terms to be P as recommended by Skalski &
% Gilliman
% constraints: a > 0, b > 0, c >= 0
function [L,F] = bd_mle(N0,N1,P,p)
% extract the parameters within p
a = p(1);
b = p(2);
c = p(3);
s = p(4);
% observations lasted 1 days
t = 1;
% solution to the ODE
Nt = exp( (log(N0) + N0.*b - P.*a.*t + P.*c.*log(N0))./(P.*c + 1) ...
     - wrightOmega(log(b./(P.*c + 1)) + (log(N0) + N0.*b - P.*a.*t + P.*c.*log(N0))./(P.*c + 1)));
F = (N0-Nt)./P; % integrated feeding rate
E = (log(F(:)) - log(N1(:))); % the difference
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0
%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
% Hassell-Varley
% Note that we have set P-1 terms to be P as recommended by Skalski &
% Gilliman
% constraints: a > 0, b > 0
function [L,F] = hv_mle(N0,N1,P,p)
a = p(1);
b = p(2);
m = p(3);
s = p(4);
% observations lasted 1 days
t = 1;
% solution to the ODE
Nt = exp((N0.*b + P.^m.*log(N0) - P.*a.*t)./P.^m - wrightOmega(log(b./P.^m) + (N0.*b + P.^m.*log(N0) - P.*a.*t)./P.^m));

F = (N0-Nt)./P; % integrated feeding rate
E = (log(F(:)) - log(N1(:))); % the difference
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0
