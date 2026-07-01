-- Seed: 7090601257515509495,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity idpkzrs is
  port (kxbdcv : linkage integer; sz : in std_logic; ykp : out std_logic_vector(2 downto 0));
end idpkzrs;

architecture h of idpkzrs is
  
begin
  -- Multi-driven assignments
  ykp <= ('X', '-', 'Z');
  ykp <= ('L', '-', 'H');
  ykp <= ('-', 'H', 'X');
  ykp <= "0U0";
end h;



-- Seed after: 5895520441794165467,6882842853887419669
