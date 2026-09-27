-- Seed: 9120648319004071116,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (z : linkage real; py : out std_logic);
end a;

architecture xdo of a is
  
begin
  -- Multi-driven assignments
  py <= '-';
  py <= py;
  py <= 'Z';
end xdo;



-- Seed after: 7868965022374689396,6379010654866854599
