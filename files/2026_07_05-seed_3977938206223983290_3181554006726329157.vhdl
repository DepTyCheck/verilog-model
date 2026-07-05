-- Seed: 3977938206223983290,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (gczm : inout time; snysq : out std_logic_vector(2 to 4));
end v;

architecture rh of v is
  
begin
  -- Single-driven assignments
  gczm <= 1_4.40234 ns;
end rh;



-- Seed after: 5216316322877645495,3181554006726329157
