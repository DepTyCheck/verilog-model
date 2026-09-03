-- Seed: 18427054843976180844,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity zdz is
  port (pqsvuptbtv : inout integer_vector(3 to 1); gno : out severity_level; diekan : out std_logic; hlri : inout integer);
end zdz;

architecture k of zdz is
  
begin
  -- Single-driven assignments
  hlri <= 21;
  gno <= gno;
  
  -- Multi-driven assignments
  diekan <= '-';
  diekan <= diekan;
end k;



-- Seed after: 821021858225980528,11127274767545411571
