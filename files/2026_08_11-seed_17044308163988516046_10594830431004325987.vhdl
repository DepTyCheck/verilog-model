-- Seed: 17044308163988516046,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (lrehscg : inout real_vector(0 to 2); s : in std_logic_vector(4 downto 2));
end c;

architecture ufud of c is
  
begin
  -- Single-driven assignments
  lrehscg <= (4_4.3_4_2_0, 2#11.01#, 3.3);
end ufud;



-- Seed after: 4453342806847317442,10594830431004325987
