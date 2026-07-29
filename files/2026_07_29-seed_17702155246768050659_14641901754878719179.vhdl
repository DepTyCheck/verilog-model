-- Seed: 17702155246768050659,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (ccwxpf : out std_logic; et : inout integer);
end i;

architecture favv of i is
  
begin
  -- Single-driven assignments
  et <= 14;
  
  -- Multi-driven assignments
  ccwxpf <= 'X';
  ccwxpf <= ccwxpf;
  ccwxpf <= ccwxpf;
  ccwxpf <= '-';
end favv;



-- Seed after: 5192264552664009220,14641901754878719179
