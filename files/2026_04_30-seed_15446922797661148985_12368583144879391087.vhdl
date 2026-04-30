-- Seed: 15446922797661148985,12368583144879391087



entity ykkd is
  port (soidepwny : buffer time; onsio : in real; c : buffer time);
end ykkd;



architecture bnyrtwaz of ykkd is
  
begin
  
end bnyrtwaz;



entity tmjsay is
  port (vrthzmlqdl : out real);
end tmjsay;



architecture su of tmjsay is
  signal uqoq : time;
  signal seddelogn : time;
  signal naavvxdsp : time;
  signal ot : time;
  signal ht : time;
  signal qq : time;
begin
  wpf : entity work.ykkd
    port map (soidepwny => qq, onsio => vrthzmlqdl, c => ht);
  rfuch : entity work.ykkd
    port map (soidepwny => ot, onsio => vrthzmlqdl, c => naavvxdsp);
  w : entity work.ykkd
    port map (soidepwny => seddelogn, onsio => vrthzmlqdl, c => uqoq);
end su;



entity elnj is
  port (gslxhj : inout time; ifemfe : out real; h : out integer; mwvg : buffer severity_level);
end elnj;



architecture kzuplzkjf of elnj is
  signal n : time;
  signal mqhyulusxi : real;
  signal fpxq : time;
begin
  wowfisdsbm : entity work.tmjsay
    port map (vrthzmlqdl => ifemfe);
  etnmwjvdmr : entity work.ykkd
    port map (soidepwny => fpxq, onsio => mqhyulusxi, c => n);
end kzuplzkjf;



entity sbb is
  port (kc : out boolean; cfuexayhd : out severity_level; hqrnb : in time);
end sbb;



architecture ixoscq of sbb is
  signal qauorxwqhp : integer;
  signal mnysy : real;
  signal zsksaenpmf : time;
begin
  iks : entity work.elnj
    port map (gslxhj => zsksaenpmf, ifemfe => mnysy, h => qauorxwqhp, mwvg => cfuexayhd);
end ixoscq;



-- Seed after: 13419473124120372219,12368583144879391087
