-- Seed: 14011808265383123698,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (xqpmace : out std_logic; fbkdch : inout string(3 downto 5));
end r;

architecture c of r is
  
begin
  -- Single-driven assignments
  fbkdch <= fbkdch;
  
  -- Multi-driven assignments
  xqpmace <= '-';
  xqpmace <= '0';
  xqpmace <= xqpmace;
end c;



-- Seed after: 11920266386528118920,13843488114570579517
