-- Seed: 2188843048250020186,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity wwnipuz is
  port (yksupdvsj : in boolean; yzsbijvhw : linkage std_logic);
end wwnipuz;

architecture zcdxfwqx of wwnipuz is
  
begin
  
end zcdxfwqx;

entity ydygt is
  port (wqpktthf : inout real);
end ydygt;

library ieee;
use ieee.std_logic_1164.all;

architecture mzbk of ydygt is
  signal pmzckig : std_logic;
  signal umv : std_logic;
  signal xrirggxwez : boolean;
begin
  ddvfgm : entity work.wwnipuz
    port map (yksupdvsj => xrirggxwez, yzsbijvhw => umv);
  sanmkexatk : entity work.wwnipuz
    port map (yksupdvsj => xrirggxwez, yzsbijvhw => umv);
  eeboshief : entity work.wwnipuz
    port map (yksupdvsj => xrirggxwez, yzsbijvhw => pmzckig);
  
  -- Multi-driven assignments
  pmzckig <= '-';
  umv <= umv;
  umv <= umv;
end mzbk;

entity vnzusfhb is
  port (wmpmqf : linkage time; tpcxzc : buffer real);
end vnzusfhb;

library ieee;
use ieee.std_logic_1164.all;

architecture zkpx of vnzusfhb is
  signal c : std_logic;
  signal pg : boolean;
begin
  txyldpcl : entity work.wwnipuz
    port map (yksupdvsj => pg, yzsbijvhw => c);
  ockbtq : entity work.ydygt
    port map (wqpktthf => tpcxzc);
  
  -- Single-driven assignments
  pg <= FALSE;
  
  -- Multi-driven assignments
  c <= '1';
end zkpx;

library ieee;
use ieee.std_logic_1164.all;

entity symajqnc is
  port (qe : out std_logic; oujpdphz : out integer);
end symajqnc;

architecture v of symajqnc is
  signal awe : real;
  signal bmfpk : time;
  signal uateesklo : real;
  signal qdzxdmmed : time;
  signal erndkgozil : real;
  signal afzu : time;
  signal tcsuqopvm : boolean;
begin
  edfkucakt : entity work.wwnipuz
    port map (yksupdvsj => tcsuqopvm, yzsbijvhw => qe);
  hnykabpqtp : entity work.vnzusfhb
    port map (wmpmqf => afzu, tpcxzc => erndkgozil);
  uqrbasggic : entity work.vnzusfhb
    port map (wmpmqf => qdzxdmmed, tpcxzc => uateesklo);
  e : entity work.vnzusfhb
    port map (wmpmqf => bmfpk, tpcxzc => awe);
  
  -- Multi-driven assignments
  qe <= qe;
  qe <= '-';
end v;



-- Seed after: 14437590958648787756,12143220691580258643
