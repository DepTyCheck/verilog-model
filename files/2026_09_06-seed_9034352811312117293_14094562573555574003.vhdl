-- Seed: 9034352811312117293,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity ogkhbcl is
  port (u : inout integer; q : buffer boolean_vector(4 downto 0); utap : linkage std_logic_vector(4 to 4); d : linkage bit);
end ogkhbcl;

architecture le of ogkhbcl is
  
begin
  -- Single-driven assignments
  q <= (FALSE, FALSE, FALSE, TRUE, TRUE);
  u <= 1;
end le;



-- Seed after: 11835106644030881884,14094562573555574003
