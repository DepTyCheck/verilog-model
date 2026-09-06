-- Seed: 13330816874886901713,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity sdzfdvz is
  port (n : out integer; j : buffer std_logic_vector(1 to 4));
end sdzfdvz;

architecture kb of sdzfdvz is
  
begin
  -- Single-driven assignments
  n <= 2#1#;
  
  -- Multi-driven assignments
  j <= ('X', 'X', 'W', 'H');
  j <= ('X', '-', '0', 'X');
  j <= ('-', '-', 'H', 'X');
  j <= j;
end kb;

entity mxsksm is
  port (dqc : linkage time; dwflyu : buffer time; custt : linkage time);
end mxsksm;

architecture bwgatn of mxsksm is
  
begin
  -- Single-driven assignments
  dwflyu <= 8#0.2_6_3_6_5# fs;
end bwgatn;



-- Seed after: 12079165481024299723,14094562573555574003
