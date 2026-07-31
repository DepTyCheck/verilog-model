-- Seed: 8111894181585664632,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity nezqethsc is
  port (asuoplqo : linkage std_logic_vector(2 to 3); jgbm : in time; nbiwp : buffer integer; po : inout integer_vector(4 to 2));
end nezqethsc;

architecture od of nezqethsc is
  
begin
  -- Single-driven assignments
  nbiwp <= nbiwp;
  po <= (others => 0);
end od;

library ieee;
use ieee.std_logic_1164.all;

entity ha is
  port (pradupsvk : buffer time; eeejdmchxd : inout std_logic);
end ha;

library ieee;
use ieee.std_logic_1164.all;

architecture skk of ha is
  signal zubv : integer_vector(4 to 2);
  signal pbbfvqce : integer;
  signal qyyygci : std_logic_vector(2 to 3);
  signal ez : integer_vector(4 to 2);
  signal vmpztdtqtb : integer;
  signal xarhtxr : time;
  signal wujop : std_logic_vector(2 to 3);
begin
  upcj : entity work.nezqethsc
    port map (asuoplqo => wujop, jgbm => xarhtxr, nbiwp => vmpztdtqtb, po => ez);
  cqwdmlpquw : entity work.nezqethsc
    port map (asuoplqo => qyyygci, jgbm => pradupsvk, nbiwp => pbbfvqce, po => zubv);
  
  -- Single-driven assignments
  pradupsvk <= 1 hr;
  xarhtxr <= pradupsvk;
  
  -- Multi-driven assignments
  eeejdmchxd <= 'U';
  qyyygci <= wujop;
  eeejdmchxd <= 'W';
  eeejdmchxd <= '0';
end skk;

library ieee;
use ieee.std_logic_1164.all;

entity mcjyesjm is
  port (lchoezm : linkage std_logic);
end mcjyesjm;

library ieee;
use ieee.std_logic_1164.all;

architecture gwabtmupcb of mcjyesjm is
  signal znnzt : integer_vector(4 to 2);
  signal ngfc : integer;
  signal ftbztb : std_logic_vector(2 to 3);
  signal bhleetcxz : integer_vector(4 to 2);
  signal vzxmdydpcc : integer;
  signal rggyw : std_logic_vector(2 to 3);
  signal mhodppxd : std_logic;
  signal luyv : integer_vector(4 to 2);
  signal iojuivnioq : integer;
  signal mesagyeqpz : time;
  signal e : std_logic_vector(2 to 3);
begin
  k : entity work.nezqethsc
    port map (asuoplqo => e, jgbm => mesagyeqpz, nbiwp => iojuivnioq, po => luyv);
  ojpwhjsyj : entity work.ha
    port map (pradupsvk => mesagyeqpz, eeejdmchxd => mhodppxd);
  kqd : entity work.nezqethsc
    port map (asuoplqo => rggyw, jgbm => mesagyeqpz, nbiwp => vzxmdydpcc, po => bhleetcxz);
  d : entity work.nezqethsc
    port map (asuoplqo => ftbztb, jgbm => mesagyeqpz, nbiwp => ngfc, po => znnzt);
  
  -- Multi-driven assignments
  e <= ftbztb;
  ftbztb <= ('0', 'X');
end gwabtmupcb;

entity gltjlok is
  port (igtxlxwi : inout real; msvfb : linkage time_vector(0 to 0));
end gltjlok;

library ieee;
use ieee.std_logic_1164.all;

architecture q of gltjlok is
  signal pruwoaizi : std_logic;
  signal qjyesan : time;
  signal volnhorza : std_logic;
  signal zjneykmtnm : time;
begin
  idxaohuscm : entity work.ha
    port map (pradupsvk => zjneykmtnm, eeejdmchxd => volnhorza);
  epdpnmma : entity work.ha
    port map (pradupsvk => qjyesan, eeejdmchxd => pruwoaizi);
  
  -- Single-driven assignments
  igtxlxwi <= 2#0.11#;
  
  -- Multi-driven assignments
  volnhorza <= 'H';
  pruwoaizi <= volnhorza;
  volnhorza <= '-';
  volnhorza <= 'U';
end q;



-- Seed after: 16364345489041391528,4177195558088809003
