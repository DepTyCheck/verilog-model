-- Seed: 7170227295060993605,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity welfvzul is
  port (mw : linkage std_logic; cj : inout std_logic; lo : in time; tuxe : inout std_logic_vector(3 downto 3));
end welfvzul;

architecture rop of welfvzul is
  
begin
  -- Multi-driven assignments
  tuxe <= "L";
  tuxe <= tuxe;
  tuxe <= (others => '-');
  tuxe <= tuxe;
end rop;



-- Seed after: 12605416832465703656,12269339630485015285
