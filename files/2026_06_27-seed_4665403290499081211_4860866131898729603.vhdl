-- Seed: 4665403290499081211,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity exg is
  port (ilcfwp : out std_logic_vector(4 to 1); pdlbe : inout time; axilbt : buffer integer);
end exg;

architecture evngkvzek of exg is
  
begin
  -- Single-driven assignments
  axilbt <= 16#E_D_6_A#;
  pdlbe <= 1 sec;
  
  -- Multi-driven assignments
  ilcfwp <= "";
  ilcfwp <= (others => '0');
  ilcfwp <= "";
  ilcfwp <= "";
end evngkvzek;



-- Seed after: 13870613605106909486,4860866131898729603
