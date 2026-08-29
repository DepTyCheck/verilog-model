-- Seed: 6240595936008274472,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity nuzlunel is
  port (elkmh : out std_logic_vector(0 downto 0); tlj : in real; mskmotevbc : linkage real; dwu : inout real_vector(0 to 2));
end nuzlunel;

architecture g of nuzlunel is
  
begin
  -- Single-driven assignments
  dwu <= (2#01000.0_0_1_1#, 2#0_1_1_0.1#, 2#0.1_0#);
  
  -- Multi-driven assignments
  elkmh <= elkmh;
  elkmh <= elkmh;
end g;



-- Seed after: 12208308378710498227,10463297573877745897
