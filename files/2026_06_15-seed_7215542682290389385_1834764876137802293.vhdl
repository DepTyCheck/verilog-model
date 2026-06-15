-- Seed: 7215542682290389385,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity xufpunq is
  port (ymsmmlyvxc : in time; vpfwuvq : inout std_logic_vector(3 to 0));
end xufpunq;

architecture sv of xufpunq is
  
begin
  -- Multi-driven assignments
  vpfwuvq <= "";
end sv;

entity rkuc is
  port (v : buffer integer_vector(4 to 2); aqrclb : in time; puzromwn : buffer time; kkcfsu : in real_vector(4 downto 4));
end rkuc;

library ieee;
use ieee.std_logic_1164.all;

architecture fx of rkuc is
  signal pqzgczngl : std_logic_vector(3 to 0);
  signal oqs : std_logic_vector(3 to 0);
begin
  ifndhu : entity work.xufpunq
    port map (ymsmmlyvxc => puzromwn, vpfwuvq => oqs);
  gvyxf : entity work.xufpunq
    port map (ymsmmlyvxc => aqrclb, vpfwuvq => pqzgczngl);
  
  -- Single-driven assignments
  puzromwn <= 1 hr;
  
  -- Multi-driven assignments
  oqs <= "";
  oqs <= (others => '0');
  oqs <= "";
  oqs <= (others => '0');
end fx;



-- Seed after: 15671679386569802939,1834764876137802293
