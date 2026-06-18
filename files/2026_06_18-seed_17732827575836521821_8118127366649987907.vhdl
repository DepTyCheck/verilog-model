-- Seed: 17732827575836521821,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity cexs is
  port (ci : inout std_logic_vector(1 to 1));
end cexs;

architecture eiwqunaicz of cexs is
  
begin
  -- Multi-driven assignments
  ci <= (others => 'U');
  ci <= "U";
  ci <= "W";
end eiwqunaicz;



-- Seed after: 12076343440723541890,8118127366649987907
