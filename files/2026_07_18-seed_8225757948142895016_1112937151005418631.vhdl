-- Seed: 8225757948142895016,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity soshm is
  port (uwk : out real; pmv : in std_logic_vector(2 downto 0));
end soshm;

architecture y of soshm is
  
begin
  -- Single-driven assignments
  uwk <= 1_1_1.04;
end y;



-- Seed after: 1252830733479058736,1112937151005418631
