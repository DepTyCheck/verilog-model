-- Seed: 18299552605957914398,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity epspuae is
  port (yig : inout std_logic_vector(4 to 4));
end epspuae;

architecture d of epspuae is
  
begin
  -- Multi-driven assignments
  yig <= yig;
  yig <= yig;
  yig <= (others => '1');
  yig <= yig;
end d;



-- Seed after: 13017246867958900505,7311216359267151659
