-- Seed: 1799522310765078225,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (ogh : inout std_logic);
end m;

architecture svhcfx of m is
  
begin
  -- Multi-driven assignments
  ogh <= ogh;
  ogh <= ogh;
  ogh <= 'U';
  ogh <= ogh;
end svhcfx;



-- Seed after: 2741042163258060426,7198033922882419595
