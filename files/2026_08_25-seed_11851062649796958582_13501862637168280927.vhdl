-- Seed: 11851062649796958582,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity zjzxcdq is
  port (s : out integer_vector(1 downto 1); exqqaupg : out std_logic_vector(0 downto 4));
end zjzxcdq;

architecture w of zjzxcdq is
  
begin
  -- Single-driven assignments
  s <= (others => 8#5_3_3_4_0#);
  
  -- Multi-driven assignments
  exqqaupg <= (others => '0');
  exqqaupg <= exqqaupg;
  exqqaupg <= exqqaupg;
  exqqaupg <= exqqaupg;
end w;



-- Seed after: 2564455343394861120,13501862637168280927
