-- Seed: 16599821294331946443,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (x : buffer std_logic; ecxv : out real; zq : linkage integer; dvlwf : buffer std_logic_vector(1 downto 2));
end v;

architecture o of v is
  
begin
  -- Single-driven assignments
  ecxv <= 16#B.B_8_4_8_F#;
  
  -- Multi-driven assignments
  dvlwf <= (others => '0');
  dvlwf <= dvlwf;
  dvlwf <= dvlwf;
end o;



-- Seed after: 11397128438451470969,10594830431004325987
