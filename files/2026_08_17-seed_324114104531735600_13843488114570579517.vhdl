-- Seed: 324114104531735600,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (vwrd : inout real; uvbp : linkage std_logic_vector(2 to 0); owsy : inout std_logic_vector(3 downto 2));
end q;

architecture pgnl of q is
  
begin
  -- Single-driven assignments
  vwrd <= vwrd;
  
  -- Multi-driven assignments
  owsy <= ('U', 'U');
  owsy <= "0Z";
end pgnl;



-- Seed after: 16054161149051685161,13843488114570579517
