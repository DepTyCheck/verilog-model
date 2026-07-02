-- Seed: 9974716207889248315,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity npffqnot is
  port (zu : linkage std_logic_vector(4 downto 2); tz : linkage real);
end npffqnot;

architecture cgjteadbiv of npffqnot is
  
begin
  
end cgjteadbiv;

library ieee;
use ieee.std_logic_1164.all;

entity nkyit is
  port (u : out std_logic_vector(2 to 4); dy : in integer; bwgfby : buffer real; hivkhvdax : in time);
end nkyit;

architecture jn of nkyit is
  
begin
  -- Single-driven assignments
  bwgfby <= 2#0_1_0.01#;
  
  -- Multi-driven assignments
  u <= ('1', 'X', '-');
end jn;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (zdijogefk : buffer std_logic_vector(0 downto 1); ruooxhd : inout std_logic);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture qbhgeuf of r is
  signal d : real;
  signal yzltxutqwp : real;
  signal dtdyswn : std_logic_vector(4 downto 2);
  signal bnapsd : time;
  signal hxwgjypgh : real;
  signal ggqyrs : integer;
  signal opxxel : std_logic_vector(2 to 4);
  signal elscby : real;
  signal otcfxu : std_logic_vector(4 downto 2);
begin
  osmyhr : entity work.npffqnot
    port map (zu => otcfxu, tz => elscby);
  fcxp : entity work.nkyit
    port map (u => opxxel, dy => ggqyrs, bwgfby => hxwgjypgh, hivkhvdax => bnapsd);
  ersbpcmkh : entity work.npffqnot
    port map (zu => dtdyswn, tz => yzltxutqwp);
  qoqqemuqcx : entity work.npffqnot
    port map (zu => otcfxu, tz => d);
  
  -- Single-driven assignments
  ggqyrs <= 1;
  bnapsd <= 2#1.0_0_1_1# ms;
  
  -- Multi-driven assignments
  ruooxhd <= 'L';
  otcfxu <= "-WL";
end qbhgeuf;



-- Seed after: 11448887827772983202,13694093582652240945
