-- Seed: 611700203488299951,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity ufre is
  port (wu : buffer std_logic_vector(2 to 4); w : out integer);
end ufre;

architecture sviiimqa of ufre is
  
begin
  -- Single-driven assignments
  w <= 1_1_3;
  
  -- Multi-driven assignments
  wu <= ('L', 'L', 'Z');
end sviiimqa;



-- Seed after: 7117840756154782379,6697892553037813751
