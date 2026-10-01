-- Seed: 7801574156490085677,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity wqkqjo is
  port (xfz : inout std_logic_vector(4 to 2));
end wqkqjo;

architecture u of wqkqjo is
  
begin
  -- Multi-driven assignments
  xfz <= (others => '0');
  xfz <= xfz;
end u;



-- Seed after: 7772477305974871830,15025465285671019065
