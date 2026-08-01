-- Seed: 8437483032037816152,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity kmt is
  port (bsbvdsid : in severity_level; mfu : out std_logic; qqv : in time);
end kmt;

architecture ecprl of kmt is
  
begin
  -- Multi-driven assignments
  mfu <= mfu;
  mfu <= 'L';
end ecprl;



-- Seed after: 1677451294840812814,4292249356257567981
