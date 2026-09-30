-- Seed: 13322459851029357676,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity rywrnygd is
  port (dlmqjeirgb : linkage std_logic_vector(0 downto 2); yioadg : out bit; fpft : linkage time; ecfupjhbk : linkage severity_level);
end rywrnygd;

architecture hzamomclg of rywrnygd is
  
begin
  -- Single-driven assignments
  yioadg <= yioadg;
end hzamomclg;

library ieee;
use ieee.std_logic_1164.all;

entity zlyv is
  port (dice : linkage std_logic_vector(1 to 4));
end zlyv;

library ieee;
use ieee.std_logic_1164.all;

architecture ubuvrbl of zlyv is
  signal ubkcn : severity_level;
  signal npxhq : time;
  signal ex : bit;
  signal afgjtb : severity_level;
  signal bkcmxv : time;
  signal thcnzedl : bit;
  signal cnf : std_logic_vector(0 downto 2);
begin
  s : entity work.rywrnygd
    port map (dlmqjeirgb => cnf, yioadg => thcnzedl, fpft => bkcmxv, ecfupjhbk => afgjtb);
  ecc : entity work.rywrnygd
    port map (dlmqjeirgb => cnf, yioadg => ex, fpft => npxhq, ecfupjhbk => ubkcn);
  
  -- Multi-driven assignments
  cnf <= cnf;
end ubuvrbl;

entity fmptgnkj is
  port (ivsebryzl : in time);
end fmptgnkj;

library ieee;
use ieee.std_logic_1164.all;

architecture est of fmptgnkj is
  signal zo : severity_level;
  signal kqaiukmc : time;
  signal ug : bit;
  signal icdg : std_logic_vector(0 downto 2);
  signal nmcu : std_logic_vector(1 to 4);
  signal oliu : severity_level;
  signal pvqerlx : time;
  signal r : bit;
  signal jqttrlrpy : std_logic_vector(0 downto 2);
begin
  xi : entity work.rywrnygd
    port map (dlmqjeirgb => jqttrlrpy, yioadg => r, fpft => pvqerlx, ecfupjhbk => oliu);
  s : entity work.zlyv
    port map (dice => nmcu);
  giyfhehxz : entity work.rywrnygd
    port map (dlmqjeirgb => icdg, yioadg => ug, fpft => kqaiukmc, ecfupjhbk => zo);
  
  -- Multi-driven assignments
  nmcu <= nmcu;
  jqttrlrpy <= "";
end est;



-- Seed after: 18070460360247150065,12260394286515585877
