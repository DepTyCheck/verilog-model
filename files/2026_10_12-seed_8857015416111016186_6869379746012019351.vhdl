-- Seed: 8857015416111016186,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity vlv is
  port (kzh : inout std_logic_vector(3 to 3));
end vlv;

architecture ul of vlv is
  
begin
  -- Multi-driven assignments
  kzh <= "X";
  kzh <= kzh;
  kzh <= "W";
  kzh <= "-";
end ul;



-- Seed after: 16519800609694402160,6869379746012019351
