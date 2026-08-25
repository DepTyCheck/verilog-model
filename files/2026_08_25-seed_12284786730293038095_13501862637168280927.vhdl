-- Seed: 12284786730293038095,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity iqhyb is
  port (fpxnsdy : out string(2 downto 5); uoao : in severity_level; d : buffer real; dhepbzpoi : inout std_logic);
end iqhyb;

architecture x of iqhyb is
  
begin
  -- Single-driven assignments
  d <= 2_3.3;
  fpxnsdy <= fpxnsdy;
end x;

entity uja is
  port (hdx : out time);
end uja;

library ieee;
use ieee.std_logic_1164.all;

architecture yzzxaadk of uja is
  signal nmmkabramb : std_logic;
  signal mju : real;
  signal xal : severity_level;
  signal mmepgrmjq : string(2 downto 5);
  signal yohl : std_logic;
  signal vgzf : real;
  signal bguyyp : severity_level;
  signal uglzjm : string(2 downto 5);
begin
  qmjtfki : entity work.iqhyb
    port map (fpxnsdy => uglzjm, uoao => bguyyp, d => vgzf, dhepbzpoi => yohl);
  e : entity work.iqhyb
    port map (fpxnsdy => mmepgrmjq, uoao => xal, d => mju, dhepbzpoi => nmmkabramb);
  
  -- Single-driven assignments
  xal <= bguyyp;
  hdx <= hdx;
  
  -- Multi-driven assignments
  yohl <= yohl;
  nmmkabramb <= 'W';
  nmmkabramb <= 'Z';
end yzzxaadk;



-- Seed after: 10160205014147707200,13501862637168280927
