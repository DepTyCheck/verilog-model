-- Seed: 9342999733294535954,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity bjd is
  port (dvrzpyx : buffer severity_level; ow : out std_logic_vector(0 to 3); fpivpfdsx : buffer severity_level);
end bjd;

architecture nbyvlh of bjd is
  
begin
  -- Single-driven assignments
  fpivpfdsx <= dvrzpyx;
  dvrzpyx <= fpivpfdsx;
  
  -- Multi-driven assignments
  ow <= ow;
  ow <= ow;
  ow <= "00Z1";
end nbyvlh;

entity tvuq is
  port (cttjg : inout string(1 to 2));
end tvuq;

library ieee;
use ieee.std_logic_1164.all;

architecture yru of tvuq is
  signal awf : severity_level;
  signal hydkhyzsl : std_logic_vector(0 to 3);
  signal iyygmmnyhu : severity_level;
  signal lpfoic : severity_level;
  signal miegebwewj : std_logic_vector(0 to 3);
  signal ebntap : severity_level;
begin
  lo : entity work.bjd
    port map (dvrzpyx => ebntap, ow => miegebwewj, fpivpfdsx => lpfoic);
  skftds : entity work.bjd
    port map (dvrzpyx => iyygmmnyhu, ow => hydkhyzsl, fpivpfdsx => awf);
  
  -- Single-driven assignments
  cttjg <= "dn";
end yru;



-- Seed after: 10995306325959934934,499459191852795575
