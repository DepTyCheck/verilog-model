-- Seed: 6550490726266620077,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity ch is
  port (k : buffer integer_vector(2 downto 1); qvjux : inout std_logic; mryv : out integer);
end ch;

architecture jdiribxqf of ch is
  
begin
  -- Single-driven assignments
  k <= (2#0_1#, 2#1000#);
  mryv <= 2#1#;
  
  -- Multi-driven assignments
  qvjux <= '-';
  qvjux <= 'X';
  qvjux <= 'L';
end jdiribxqf;

library ieee;
use ieee.std_logic_1164.all;

entity blc is
  port (vtfa : buffer bit_vector(0 downto 2); k : buffer std_logic; v : in std_logic_vector(0 downto 4));
end blc;

architecture f of blc is
  
begin
  -- Multi-driven assignments
  k <= 'H';
  k <= '1';
end f;



-- Seed after: 10409170990126290650,13479070923501788437
