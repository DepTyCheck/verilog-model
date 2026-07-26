-- Seed: 3780568821423752884,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity gtq is
  port (vcbc : out std_logic_vector(2 to 2));
end gtq;

architecture xkg of gtq is
  
begin
  -- Multi-driven assignments
  vcbc <= "L";
  vcbc <= vcbc;
  vcbc <= "W";
end xkg;



-- Seed after: 10102703275332539283,7808623373429384027
