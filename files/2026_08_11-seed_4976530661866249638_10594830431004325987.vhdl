-- Seed: 4976530661866249638,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity qpfmohd is
  port (hxaltmbls : out std_logic; k : in std_logic_vector(4 to 4); p : in time_vector(3 to 1));
end qpfmohd;

architecture rly of qpfmohd is
  
begin
  -- Multi-driven assignments
  hxaltmbls <= hxaltmbls;
end rly;

entity itcamrcjza is
  port (h : out time_vector(4 to 0); aidirplli : out integer; eamuk : inout time);
end itcamrcjza;

library ieee;
use ieee.std_logic_1164.all;

architecture sz of itcamrcjza is
  signal mymvk : std_logic_vector(4 to 4);
  signal ytedqkvgn : std_logic;
begin
  trngp : entity work.qpfmohd
    port map (hxaltmbls => ytedqkvgn, k => mymvk, p => h);
  mfsaskrtd : entity work.qpfmohd
    port map (hxaltmbls => ytedqkvgn, k => mymvk, p => h);
  
  -- Single-driven assignments
  aidirplli <= 1_3_1_3;
  h <= (others => 0 ns);
  eamuk <= 14 ps;
end sz;

library ieee;
use ieee.std_logic_1164.all;

entity wu is
  port (hwati : out std_logic_vector(1 to 0); cwnzfuurc : buffer character; edniju : linkage integer);
end wu;

architecture rtzriqp of wu is
  signal qsy : time;
  signal evfvvctefe : integer;
  signal drmby : time_vector(4 to 0);
  signal mftje : time;
  signal julbopw : integer;
  signal wmx : time_vector(4 to 0);
begin
  ukdzxoyux : entity work.itcamrcjza
    port map (h => wmx, aidirplli => julbopw, eamuk => mftje);
  dpkjim : entity work.itcamrcjza
    port map (h => drmby, aidirplli => evfvvctefe, eamuk => qsy);
  
  -- Single-driven assignments
  cwnzfuurc <= cwnzfuurc;
end rtzriqp;



-- Seed after: 2185372798013687237,10594830431004325987
