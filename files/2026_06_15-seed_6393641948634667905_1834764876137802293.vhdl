-- Seed: 6393641948634667905,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity sg is
  port (fbrvzco : in std_logic_vector(3 downto 3));
end sg;

architecture nvqukqf of sg is
  
begin
  
end nvqukqf;

entity keuk is
  port (d : buffer boolean; naofmf : inout integer);
end keuk;

library ieee;
use ieee.std_logic_1164.all;

architecture won of keuk is
  signal bwgxwgu : std_logic_vector(3 downto 3);
begin
  etq : entity work.sg
    port map (fbrvzco => bwgxwgu);
  
  -- Single-driven assignments
  naofmf <= 3_1_2_2_2;
  
  -- Multi-driven assignments
  bwgxwgu <= (others => '-');
  bwgxwgu <= "-";
  bwgxwgu <= (others => '-');
  bwgxwgu <= "0";
end won;



-- Seed after: 9780901743960902453,1834764876137802293
