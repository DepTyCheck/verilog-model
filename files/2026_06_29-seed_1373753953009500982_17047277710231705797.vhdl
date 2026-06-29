-- Seed: 1373753953009500982,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity vignadrrth is
  port (vbb : out real; m : inout std_logic);
end vignadrrth;

architecture jj of vignadrrth is
  
begin
  -- Single-driven assignments
  vbb <= 8#155.01#;
  
  -- Multi-driven assignments
  m <= '1';
  m <= '1';
  m <= 'H';
  m <= 'W';
end jj;



-- Seed after: 6667310697527872415,17047277710231705797
