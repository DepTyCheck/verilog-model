-- Seed: 491576612621045583,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity mbhzamx is
  port (jcnxsnlh : buffer std_logic);
end mbhzamx;

architecture zqriy of mbhzamx is
  
begin
  -- Multi-driven assignments
  jcnxsnlh <= '0';
end zqriy;

library ieee;
use ieee.std_logic_1164.all;

entity qjhg is
  port (ovsrj : inout std_logic; dwmquhnwq : out std_logic; zwn : out std_logic);
end qjhg;

library ieee;
use ieee.std_logic_1164.all;

architecture ngvs of qjhg is
  signal hp : std_logic;
begin
  yxxyfqamnh : entity work.mbhzamx
    port map (jcnxsnlh => zwn);
  wpp : entity work.mbhzamx
    port map (jcnxsnlh => hp);
  
  -- Multi-driven assignments
  ovsrj <= '1';
  zwn <= hp;
  hp <= 'X';
end ngvs;



-- Seed after: 438184643347297706,11127274767545411571
