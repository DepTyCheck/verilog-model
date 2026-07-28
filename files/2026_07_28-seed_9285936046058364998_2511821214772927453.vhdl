-- Seed: 9285936046058364998,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity eufmw is
  port (srk : linkage time; ikuy : buffer time; otsbfhgmhr : in std_logic; tcawfjf : in integer);
end eufmw;

architecture bpnyjnh of eufmw is
  
begin
  -- Single-driven assignments
  ikuy <= 1 sec;
end bpnyjnh;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (pq : linkage std_logic_vector(4 to 4); r : inout time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture tz of w is
  signal h : std_logic;
  signal ukpt : time;
  signal llkstuyff : integer;
  signal ktm : std_logic;
  signal pwfoll : time;
  signal yxrmk : time;
  signal szinquie : integer;
  signal t : std_logic;
  signal rwbtyodpx : time;
  signal d : time;
begin
  edulhdtupz : entity work.eufmw
    port map (srk => d, ikuy => rwbtyodpx, otsbfhgmhr => t, tcawfjf => szinquie);
  wdwohn : entity work.eufmw
    port map (srk => yxrmk, ikuy => pwfoll, otsbfhgmhr => ktm, tcawfjf => llkstuyff);
  lrjhk : entity work.eufmw
    port map (srk => ukpt, ikuy => r, otsbfhgmhr => h, tcawfjf => szinquie);
end tz;

entity ar is
  port (hhxizv : inout integer; doyde : out time_vector(1 downto 0));
end ar;

library ieee;
use ieee.std_logic_1164.all;

architecture spba of ar is
  signal dhaa : integer;
  signal othskkz : std_logic;
  signal zfhbjz : time;
  signal frygtfnll : time;
begin
  jfijbzf : entity work.eufmw
    port map (srk => frygtfnll, ikuy => zfhbjz, otsbfhgmhr => othskkz, tcawfjf => dhaa);
  
  -- Single-driven assignments
  dhaa <= hhxizv;
  hhxizv <= dhaa;
  doyde <= (2#0# ms, 0 sec);
  
  -- Multi-driven assignments
  othskkz <= othskkz;
  othskkz <= '1';
  othskkz <= othskkz;
end spba;



-- Seed after: 13329488660963107416,2511821214772927453
