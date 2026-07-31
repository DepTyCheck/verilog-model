-- Seed: 13438159207981603739,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (xi : out time; chkavyuy : buffer std_logic_vector(4 downto 0));
end d;

architecture mehij of d is
  
begin
  -- Single-driven assignments
  xi <= 16#F0194.5# ns;
  
  -- Multi-driven assignments
  chkavyuy <= ('Z', 'W', 'W', '-', '-');
end mehij;



-- Seed after: 11546767986707750146,4177195558088809003
