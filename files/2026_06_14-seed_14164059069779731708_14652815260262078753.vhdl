-- Seed: 14164059069779731708,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ix is
  port (iyso : in std_logic; e : linkage integer; sk : buffer std_logic);
end ix;

architecture fwk of ix is
  
begin
  -- Multi-driven assignments
  sk <= 'U';
  sk <= 'L';
  sk <= 'X';
  sk <= 'U';
end fwk;



-- Seed after: 4165022982898445543,14652815260262078753
