-- Seed: 5643978057417342879,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity ej is
  port (otcgn : buffer std_logic_vector(0 downto 1); arit : out integer_vector(4 to 3));
end ej;

architecture nb of ej is
  
begin
  -- Single-driven assignments
  arit <= (others => 0);
  
  -- Multi-driven assignments
  otcgn <= "";
  otcgn <= (others => '0');
  otcgn <= "";
end nb;



-- Seed after: 17602531868824224667,14629254427735353553
