-- Seed: 3075958333963181483,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity xshx is
  port (m : out std_logic_vector(2 to 2); wlqucb : out severity_level);
end xshx;

architecture lr of xshx is
  
begin
  -- Single-driven assignments
  wlqucb <= WARNING;
  
  -- Multi-driven assignments
  m <= (others => 'X');
  m <= "1";
  m <= m;
end lr;



-- Seed after: 9924524552205063789,8927267689619684183
