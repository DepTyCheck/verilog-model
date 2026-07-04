-- Seed: 15579351944468292904,6290177331721581829

use std.reflection.all;

entity hjxqsfnn is
  port (ofzq : buffer integer; etzjayrmv : inout subtype_mirror; wrtljfritt : out boolean);
end hjxqsfnn;

architecture gh of hjxqsfnn is
  
begin
  -- Single-driven assignments
  ofzq <= 2211;
  wrtljfritt <= FALSE;
end gh;

use std.reflection.all;

entity gyig is
  port (akpn : inout file_value_mirror; htmuokez : out time);
end gyig;

use std.reflection.all;

architecture mj of gyig is
  signal nj : boolean;
  shared variable tohruz : subtype_mirror;
  signal ylomvlx : integer;
  signal wpjxbhg : boolean;
  shared variable rvfidg : subtype_mirror;
  signal mztiqqav : integer;
  signal mlxzn : boolean;
  shared variable nvtye : subtype_mirror;
  signal zvf : integer;
  signal cghakguzd : boolean;
  shared variable sonzj : subtype_mirror;
  signal derya : integer;
begin
  yprbjmupsq : entity work.hjxqsfnn
    port map (ofzq => derya, etzjayrmv => sonzj, wrtljfritt => cghakguzd);
  jayalycavq : entity work.hjxqsfnn
    port map (ofzq => zvf, etzjayrmv => nvtye, wrtljfritt => mlxzn);
  h : entity work.hjxqsfnn
    port map (ofzq => mztiqqav, etzjayrmv => rvfidg, wrtljfritt => wpjxbhg);
  bpdcofo : entity work.hjxqsfnn
    port map (ofzq => ylomvlx, etzjayrmv => tohruz, wrtljfritt => nj);
end mj;



-- Seed after: 16729082523838013052,6290177331721581829
