-- Seed: 14663657501469972045,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity ijm is
  port (yag : out std_logic; j : buffer real; uavb : in character; ftppg : buffer real);
end ijm;

architecture mh of ijm is
  
begin
  -- Single-driven assignments
  j <= ftppg;
  ftppg <= j;
  
  -- Multi-driven assignments
  yag <= 'W';
  yag <= yag;
  yag <= 'L';
end mh;



-- Seed after: 877243120329541710,10754487200446211253
