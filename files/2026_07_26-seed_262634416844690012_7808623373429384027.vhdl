-- Seed: 262634416844690012,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity fb is
  port (k : out std_logic_vector(3 to 1); xq : linkage time; uid : linkage integer; fotfit : in time);
end fb;

architecture d of fb is
  
begin
  -- Multi-driven assignments
  k <= (others => '0');
  k <= k;
  k <= "";
end d;



-- Seed after: 861218572739358005,7808623373429384027
