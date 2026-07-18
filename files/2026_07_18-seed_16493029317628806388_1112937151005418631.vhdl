-- Seed: 16493029317628806388,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity qzj is
  port (atlu : inout std_logic_vector(2 to 4); y : out std_logic_vector(1 to 2));
end qzj;

architecture tefzcx of qzj is
  
begin
  -- Multi-driven assignments
  y <= ('U', 'Z');
  y <= "HL";
  y <= y;
end tefzcx;

library ieee;
use ieee.std_logic_1164.all;

entity lip is
  port (s : out std_logic_vector(2 to 4));
end lip;

architecture ftpbf of lip is
  
begin
  -- Multi-driven assignments
  s <= s;
  s <= s;
  s <= ('Z', 'W', '1');
end ftpbf;



-- Seed after: 9330856255296404801,1112937151005418631
