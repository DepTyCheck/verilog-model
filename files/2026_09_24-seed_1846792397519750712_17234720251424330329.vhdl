-- Seed: 1846792397519750712,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity ooeoj is
  port (qqcd : inout std_logic_vector(4 to 4); wdxi : in time_vector(3 to 3); y : inout real; dt : out time);
end ooeoj;

architecture g of ooeoj is
  
begin
  -- Single-driven assignments
  dt <= 02 fs;
  y <= 2#0_1_1_0_1.1001#;
  
  -- Multi-driven assignments
  qqcd <= qqcd;
  qqcd <= "Z";
  qqcd <= "U";
  qqcd <= (others => '0');
end g;



-- Seed after: 6282740925206826067,17234720251424330329
