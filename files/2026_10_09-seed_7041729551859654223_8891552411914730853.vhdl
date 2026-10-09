-- Seed: 7041729551859654223,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity we is
  port (hpwry : out std_logic_vector(3 downto 1));
end we;

architecture v of we is
  
begin
  -- Multi-driven assignments
  hpwry <= "01U";
end v;



-- Seed after: 4536128750357832211,8891552411914730853
