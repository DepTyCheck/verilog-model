-- Seed: 12625205586089752461,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity jy is
  port (wfo : in std_logic; qecg : out time; bacmjo : out std_logic_vector(2 to 4));
end jy;

architecture idiykjsfzh of jy is
  
begin
  -- Single-driven assignments
  qecg <= 0_3_2_1 us;
  
  -- Multi-driven assignments
  bacmjo <= ('U', 'U', 'Z');
  bacmjo <= "0X-";
  bacmjo <= "WUX";
  bacmjo <= ('-', '1', 'H');
end idiykjsfzh;



-- Seed after: 13496671387506395367,14652815260262078753
