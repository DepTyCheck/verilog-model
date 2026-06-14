-- Seed: 7187332830890077767,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ltpw is
  port (x : buffer real_vector(3 downto 2); nyld : out integer_vector(0 downto 2); msgs : out std_logic);
end ltpw;

architecture uofhdmii of ltpw is
  
begin
  -- Single-driven assignments
  nyld <= (others => 0);
  
  -- Multi-driven assignments
  msgs <= '0';
end uofhdmii;



-- Seed after: 1162316182142341159,14652815260262078753
