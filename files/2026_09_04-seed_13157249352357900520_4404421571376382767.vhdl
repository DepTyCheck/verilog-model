-- Seed: 13157249352357900520,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity gsc is
  port (px : out std_logic_vector(2 downto 2); pvfjo : out time_vector(4 to 4));
end gsc;

architecture rmehm of gsc is
  
begin
  -- Single-driven assignments
  pvfjo <= pvfjo;
  
  -- Multi-driven assignments
  px <= "1";
  px <= px;
  px <= px;
  px <= px;
end rmehm;



-- Seed after: 14507071835315614198,4404421571376382767
