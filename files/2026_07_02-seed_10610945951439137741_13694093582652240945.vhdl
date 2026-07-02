-- Seed: 10610945951439137741,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity aurdd is
  port (iuch : inout real; kf : out std_logic);
end aurdd;

architecture glibsw of aurdd is
  
begin
  -- Multi-driven assignments
  kf <= 'U';
  kf <= 'W';
  kf <= '0';
end glibsw;



-- Seed after: 5136684946640087429,13694093582652240945
