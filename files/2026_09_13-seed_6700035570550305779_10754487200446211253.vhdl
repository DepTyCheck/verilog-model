-- Seed: 6700035570550305779,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity ax is
  port (a : out std_logic);
end ax;

architecture i of ax is
  
begin
  -- Multi-driven assignments
  a <= a;
  a <= 'X';
  a <= a;
  a <= 'U';
end i;



-- Seed after: 15134127127440822987,10754487200446211253
