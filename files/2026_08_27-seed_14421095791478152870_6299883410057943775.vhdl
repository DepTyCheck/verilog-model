-- Seed: 14421095791478152870,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (fea : linkage integer; lymv : inout std_logic_vector(0 to 3); pnb : inout time);
end c;

architecture nadlhyps of c is
  
begin
  -- Multi-driven assignments
  lymv <= ('H', 'H', '-', '1');
  lymv <= "L0W0";
end nadlhyps;



-- Seed after: 7370460047054366185,6299883410057943775
