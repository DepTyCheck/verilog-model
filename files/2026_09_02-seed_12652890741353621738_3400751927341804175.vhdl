-- Seed: 12652890741353621738,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity sgxcf is
  port (wanlvp : buffer std_logic_vector(4 downto 0));
end sgxcf;

architecture w of sgxcf is
  
begin
  -- Multi-driven assignments
  wanlvp <= ('H', 'L', '0', 'Z', 'L');
end w;



-- Seed after: 4072470527675366621,3400751927341804175
