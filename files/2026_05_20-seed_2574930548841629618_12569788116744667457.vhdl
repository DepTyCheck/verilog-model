-- Seed: 2574930548841629618,12569788116744667457



entity cezbidws is
  port (htg : inout integer; qzp : linkage severity_level; nuwqyds : inout integer);
end cezbidws;



architecture jzu of cezbidws is
  
begin
  
end jzu;



entity kongd is
  port (kveffciqq : out time);
end kongd;



architecture qqahxfh of kongd is
  signal ydp : integer;
  signal va : integer;
  signal ktzzxeytbb : integer;
  signal ha : severity_level;
  signal ya : integer;
begin
  ud : entity work.cezbidws
    port map (htg => ya, qzp => ha, nuwqyds => ktzzxeytbb);
  csuafhqmyz : entity work.cezbidws
    port map (htg => va, qzp => ha, nuwqyds => ydp);
end qqahxfh;



entity iebl is
  port (m : inout real; a : buffer boolean; p : inout integer);
end iebl;



architecture wghrw of iebl is
  signal slawapdw : time;
  signal ptqfpigk : integer;
  signal mggvzbv : severity_level;
  signal zuylvyo : integer;
  signal akrdnli : severity_level;
  signal qrrqsddik : integer;
begin
  ocyosjvspx : entity work.cezbidws
    port map (htg => qrrqsddik, qzp => akrdnli, nuwqyds => p);
  rz : entity work.cezbidws
    port map (htg => zuylvyo, qzp => mggvzbv, nuwqyds => ptqfpigk);
  zuv : entity work.kongd
    port map (kveffciqq => slawapdw);
end wghrw;



-- Seed after: 1961999522266820926,12569788116744667457
