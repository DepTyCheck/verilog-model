-- Seed: 15402185924529820519,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (f : linkage std_logic_vector(2 downto 0); xkho : in real; ifin : out boolean_vector(1 to 4));
end w;

architecture pq of w is
  
begin
  -- Single-driven assignments
  ifin <= (FALSE, TRUE, TRUE, FALSE);
end pq;



-- Seed after: 12020979435668144404,10463297573877745897
