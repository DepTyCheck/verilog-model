-- Seed: 3681119662770365792,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (x : buffer std_logic);
end c;

architecture fmwtiales of c is
  
begin
  -- Multi-driven assignments
  x <= x;
  x <= x;
  x <= 'X';
end fmwtiales;



-- Seed after: 3221968680561575434,10754487200446211253
