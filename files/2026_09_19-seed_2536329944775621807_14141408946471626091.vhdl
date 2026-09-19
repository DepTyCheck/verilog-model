-- Seed: 2536329944775621807,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity tqfs is
  port (ffq : inout std_logic; w : linkage time_vector(4 to 4));
end tqfs;

architecture ulhximtdcn of tqfs is
  
begin
  -- Multi-driven assignments
  ffq <= 'X';
  ffq <= ffq;
  ffq <= ffq;
  ffq <= '1';
end ulhximtdcn;



-- Seed after: 1053227681367729442,14141408946471626091
