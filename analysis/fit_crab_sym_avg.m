function fit_crab_sym

% Fit functional response models to crab foraging data, where data are
% averages for each trial
% With Adrian Stier, Feb 2019

% Modified from bluehead code by JW White

% Read in data
X = importdata('avg_crab_trial_data_for_ML.csv');


% Define some variables
Pred = X.data(:,2);


%Pred = Pred + min(Pred(Pred>0));
%Pred = Pred + 0.1;
Dead = X.data(:,4);
Sett = X.data(:,3);

%Dens = X.data(:,4);
%Rugo = X.data(:,5);

Surv = Sett-Dead;


% Integrated feeding rate = (N(0) - N(t))/P
IntF = Dead./Pred;

% Problem: get bad values if Dead = 0 or Pred = 0
% Apply corrections:
minI = min(IntF(IntF>0));
maxI = max(IntF(~isinf(IntF)));
IntF = min(IntF,maxI);
IntF = max(IntF,minI);

Pred(Pred==0) = 1e-3;

% Place lower bound on handling time (can't be exactly 0)
hmin = 1e-10;

% Which models to run?
doH2 = true;
doCM = true;
doBD = true;
doHV = true;
doRD = true;

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plotting the data: coplot
Psub(:,1) = Pred==1;
Psub(:,2) = Pred==2;
Psub(:,3) = Pred==3;

figure(1)
clf
set(gcf,'position',[50 500 750 400])
%clf
% top: plot predator densities
subplot(2,3,1:3)
hold on
Marker = {'ko','k^','kd'};
for i = 1:3
    jit = (rand(sum(Psub(:,i)),1)-0.5).*0.1;
plot(Pred(Psub(:,i))+jit,ones(sum(Psub(:,i)),1)+i,Marker{i},'markersize',10)
end
xlim([0 3])
ylim([1.9 4.1])
set(gca,'xtick',1:7,'ytick',2:4,'yticklabel',[],'tickdir','out','ticklength',[0.015 0.015])
set(gca,'position',[0.13 0.7 0.78 0.2])
xlabel('Predator density')

for i = 1:3
    subplot(2,3,i+3)
    hold on
    jit = (rand(sum(Psub(:,i)),1)-0.5).*0.2;
    plot(Sett(Psub(:,i))+jit,IntF(Psub(:,i)),Marker{i})
    xlim([0 22])
    ylim([0 max(Dead)+2])
    set(gca,'xtick',[0 5 10 15 20],'ytick',0:2:max(Dead)+2)
    set(gca,'tickdir','out','ticklength',[0.02 0.02])
    
    if i == 2
    xlabel('Initial prey density')
    end
    if i == 1
    ylabel('Integrated feeding rate')
    end
end
% End plotting data
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) Holling type-II
if doH2
% Do some fits to find starting values
p0 = [0.3, 0.1, std(Dead)];
Xtmp  = 0:30;
n = length(Xtmp);
for i = 1:3
    Ptmp = mean(Pred(Psub(:,i)));
[~,F] = h2_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p0); 
    subplot(2,3,i+3)
    hold on
 %   plot(Xtmp,F,'r-')
end


lb = [0, hmin, 0];
ub = [Inf,Inf, Inf];
options = optimset('Algorithm','interior-point','Display','iter');
[p_h2,L_h2,~,~,~,~,H_h2] = fmincon(@(p)h2_mle(Sett,IntF,Pred,p),p0,[],[],[],[],lb,ub,[],options); 

% Plot the solution
for i = 1:3
    Ptmp = median(Pred(Psub(:,i)));
[~,F] = h2_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p_h2);
    subplot(2,3,i+3)
    hold on
    plot(Xtmp,F,'b-')
end
% end Holling type II
end % end if doH2
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) Crowley-Martin
if doCM
% Do some fits to find starting values
p0 = [0.5, 0.1, 1, std(Dead)];
Xtmp  = 0:30;
n = length(Xtmp);
for i = 1:3
    Ptmp = mean(Pred(Psub(:,i)));
[~,F] = cm_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p0);
    subplot(2,3,i+3)
    hold on
  %  plot(Xtmp,F,'r-')
end


lb = [0, hmin, -Inf, 0];
ub = [Inf,Inf, Inf, Inf];
options = optimset('Algorithm','sqp','Display','iter');
[p_cm,L_cm,~,~,~,~,H_cm] = fmincon(@(p)cm_mle(Sett,IntF,Pred,p),p0,[],[],[],[],lb,ub,[],options); 

% Plot the solution
for i = 1:3
    Ptmp = median(Pred(Psub(:,i)));
[~,F] = cm_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p_cm);
    subplot(2,3,i+3)
    hold on
    plot(Xtmp,F,'m')
end
% end Crowley-Martin
end % end if doCM
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3) Beddington-DeAngelis
if doBD
% Do some fits to find starting values
p0 = [0.5, 0.1, 1, std(Dead)];
Xtmp  = 0:30;
n = length(Xtmp);
for i = 1:3
    Ptmp = mean(Pred(Psub(:,i)));
[~,F] = bd_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p0);
    subplot(2,3,i+3)
    hold on
%    plot(Xtmp,F,'r-')
end

lb = [0, hmin, realmin, 0];
ub = [Inf,Inf, Inf, Inf];
options = optimset('Algorithm','sqp','Display','iter');
[p_bd,L_bd,~,~,~,~,H_bd] = fmincon(@(p)bd_mle(Sett,IntF,Pred,p),p0,[],[],[],[],lb,ub,[],options); 

% Plot the solution
for i = 1:3
    Ptmp = median(Pred(Psub(:,i)));
[~,F] = bd_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p_bd);
    subplot(2,3,i+3)
    hold on
    plot(Xtmp,F,'r')
end
% end Beddington-DeAngelis
end % end if doBD
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4) Hassell-Varley
if doHV
% Do some fits to find starting values
p0 = [0.5, 0.1, 1, std(Dead)];
Xtmp  = 0:30;
n = length(Xtmp);
for i = 1:3
    Ptmp = mean(Pred(Psub(:,i)));
[~,F] = hv_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p0);
    subplot(2,3,i+3)
    hold on
  %  plot(Xtmp,F,'r-')
end


lb = [0, hmin, -1e2, 0];
ub = [Inf,Inf, 1e2, Inf];
options = optimset('Algorithm','sqp','Display','iter');
[p_hv,L_hv,~,~,~,~,H_hv] = fmincon(@(p)hv_mle(Sett,IntF,Pred,p),p0,[],[],[],[],lb,ub,[],options); 

% Plot the solution
for i = 1:3
    Ptmp = median(Pred(Psub(:,i)));
[~,F] = hv_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p_hv);
    subplot(2,3,i+3)
    hold on
    plot(Xtmp,F,'k')
end
% end Hassell-Varley
end % end if doHV
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5) Ratio Dependent
if doRD
% Do some fits to find starting values
p0 = [0.5, 0.1, 1, std(Dead)];
Xtmp  = 0:30;
n = length(Xtmp);
for i = 1:3
    Ptmp = mean(Pred(Psub(:,i)));
[~,F] = hv_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p0);
    subplot(2,3,i+3)
    hold on
  %  plot(Xtmp,F,'r-')
end


lb = [0, hmin, 1, 0];
ub = [Inf,Inf, 1, Inf];
options = optimset('Algorithm','sqp','Display','iter');
[p_rd,L_rd,~,~,~,~,H_rd] = fmincon(@(p)hv_mle(Sett,IntF,Pred,p),p0,[],[],[],[],lb,ub,[],options); 

% Plot the solution
for i = 1:3
    Ptmp = median(Pred(Psub(:,i)));
[~,F] = hv_mle(Xtmp(:),nan(n,1),ones(n,1).*Ptmp,p_rd);
    subplot(2,3,i+3)
    hold on
    plot(Xtmp,F,'g')
end
% end Ratio-Dependent
end % end if doRD
%%%%%%%%%%%%%%%%%%%%%%%%%%%

for i = 1:3
    subplot(2,3,i+3)
    pos = get(gca,'position');
    set(gca,'position',[pos(1) 0.15 pos(3) 0.4])
end

% Do model selection:
% Likelihoods
Ls = [L_h2; L_cm; L_bd; L_hv; L_rd];
% Number of parameters
ks = [2; 3; 3; 3; 2];
% sample size
nn = length(Pred);
% AICc
AICc = 2*Ls + 2*ks + 2.*ks.*(ks+1)./(nn - ks - 1);
dAICc = AICc - min(AICc);
% AIC weights
w1 = exp(-dAICc./2);
w = w1./sum(w1);

keyboard

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
%s = std(E);
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0



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
%Nt = exp((N0.*b + P.^m.*log(N0) - P.*a.*t)./P.^m - wrightOmega(log(b./P.^m) + (N0.*b + P.^m.*log(N0) - P.*a.*t)./P.^m));
%F = (N0-Nt)./P; % integrated feeding rate


dN = a.*N0.*P./(1 + b.*N0 + c.*P + b.*c.*N0.*P); % rate of consumption
F = dN.*t./P; % total number eaten per predator
E = (log(F(:)) - log(N1(:))); % the difference
L = -sum(log(normpdf(E,0,s)));
% constraints: a > 0, b > 0



