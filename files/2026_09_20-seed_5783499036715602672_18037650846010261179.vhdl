-- Seed: 5783499036715602672,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity axbp is
  port (cge : out std_logic_vector(0 downto 4); ezv : out boolean; cm : buffer bit_vector(4 to 3));
end axbp;

architecture aegcfmsjo of axbp is
  
begin
  -- Single-driven assignments
  cm <= (others => '0');
  
  -- Multi-driven assignments
  cge <= cge;
end aegcfmsjo;



-- Seed after: 3374376241675364049,18037650846010261179
