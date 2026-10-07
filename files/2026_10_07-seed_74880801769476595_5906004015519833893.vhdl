-- Seed: 74880801769476595,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity vb is
  port (gytr : buffer time; wbxpqev : in std_logic; uvvprupb : buffer std_logic_vector(1 to 2));
end vb;

architecture cp of vb is
  
begin
  -- Single-driven assignments
  gytr <= 2#0_0_1_1# ns;
  
  -- Multi-driven assignments
  uvvprupb <= "0X";
  uvvprupb <= ('H', '-');
  uvvprupb <= uvvprupb;
  uvvprupb <= ('W', 'Z');
end cp;



-- Seed after: 1567425704169135622,5906004015519833893
