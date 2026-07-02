-- Seed: 7113767252435552299,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity injmmesuk is
  port (urxcrkm : buffer boolean; dazzou : in time; xztwqit : out bit; bfaihv : buffer std_logic_vector(1 downto 0));
end injmmesuk;

architecture k of injmmesuk is
  
begin
  -- Single-driven assignments
  urxcrkm <= TRUE;
  xztwqit <= '1';
  
  -- Multi-driven assignments
  bfaihv <= "1H";
end k;



-- Seed after: 9377639885168931736,13694093582652240945
