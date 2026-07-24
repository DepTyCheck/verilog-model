-- Seed: 15449389224234190771,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity cn is
  port (q : inout std_logic);
end cn;

architecture onk of cn is
  
begin
  -- Multi-driven assignments
  q <= '0';
  q <= 'L';
  q <= q;
end onk;



-- Seed after: 17511174974048620986,16461708287571398341
