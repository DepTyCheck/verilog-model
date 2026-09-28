-- Seed: 12921982846798101859,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity hka is
  port (lmj : inout std_logic_vector(4 downto 1));
end hka;

architecture o of hka is
  
begin
  -- Multi-driven assignments
  lmj <= "1UH-";
  lmj <= lmj;
  lmj <= lmj;
end o;



-- Seed after: 18100392121074798049,7311216359267151659
