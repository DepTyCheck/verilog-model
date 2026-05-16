-- Seed: 16962442337727242755,11218062946904572163



entity hzqlofst is
  port (arozsxgew : in time; n : out boolean; zgxqjpvsfw : out boolean);
end hzqlofst;



architecture jxrcdppcox of hzqlofst is
  
begin
  
end jxrcdppcox;



entity pqhwbpo is
  port (y : buffer real; pt : in integer);
end pqhwbpo;



architecture ymu of pqhwbpo is
  signal qtqzvq : boolean;
  signal nyhmoxmr : boolean;
  signal j : time;
begin
  gnmwarcq : entity work.hzqlofst
    port map (arozsxgew => j, n => nyhmoxmr, zgxqjpvsfw => qtqzvq);
end ymu;



entity fziaistwjh is
  port (zjd : inout real);
end fziaistwjh;



architecture kzorbiyd of fziaistwjh is
  signal d : boolean;
  signal mzx : boolean;
  signal gu : boolean;
  signal xtbvyfeobx : boolean;
  signal du : time;
  signal b : integer;
begin
  jcsymtwb : entity work.pqhwbpo
    port map (y => zjd, pt => b);
  ynvqotszz : entity work.hzqlofst
    port map (arozsxgew => du, n => xtbvyfeobx, zgxqjpvsfw => gu);
  ldiuz : entity work.hzqlofst
    port map (arozsxgew => du, n => mzx, zgxqjpvsfw => d);
end kzorbiyd;



-- Seed after: 2822780552728384244,11218062946904572163
