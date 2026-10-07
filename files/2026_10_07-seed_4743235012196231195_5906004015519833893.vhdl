-- Seed: 4743235012196231195,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity ou is
  port (bsq : out std_logic_vector(1 downto 1));
end ou;

architecture g of ou is
  
begin
  -- Multi-driven assignments
  bsq <= (others => '1');
  bsq <= bsq;
  bsq <= bsq;
end g;



-- Seed after: 3655854063262456975,5906004015519833893
