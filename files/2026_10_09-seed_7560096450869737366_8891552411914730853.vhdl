-- Seed: 7560096450869737366,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity nhujg is
  port (kuy : buffer std_logic_vector(4 downto 3));
end nhujg;

architecture t of nhujg is
  
begin
  -- Multi-driven assignments
  kuy <= ('U', 'X');
  kuy <= kuy;
  kuy <= kuy;
  kuy <= kuy;
end t;



-- Seed after: 18147986036989327401,8891552411914730853
