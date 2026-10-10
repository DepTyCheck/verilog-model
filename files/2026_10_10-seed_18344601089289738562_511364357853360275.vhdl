-- Seed: 18344601089289738562,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity uc is
  port (btarisuyo : inout std_logic_vector(4 downto 3));
end uc;

architecture kcqzk of uc is
  
begin
  -- Multi-driven assignments
  btarisuyo <= ('H', 'X');
  btarisuyo <= btarisuyo;
end kcqzk;



-- Seed after: 6952183976721248145,511364357853360275
