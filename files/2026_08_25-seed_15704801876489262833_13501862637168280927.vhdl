-- Seed: 15704801876489262833,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ndanu is
  port (zare : inout boolean_vector(3 to 1); vjt : out std_logic_vector(2 downto 0); eip : inout boolean_vector(0 to 2));
end ndanu;

architecture im of ndanu is
  
begin
  -- Single-driven assignments
  eip <= (FALSE, TRUE, FALSE);
end im;



-- Seed after: 17776631600183292599,13501862637168280927
