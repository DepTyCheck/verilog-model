-- Seed: 1196137300731318958,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity fvunb is
  port (t : in real; vhgjcddt : in time; wd : linkage std_logic_vector(2 downto 2); ysovbs : buffer integer);
end fvunb;

architecture cbsidy of fvunb is
  
begin
  
end cbsidy;

entity eeprbma is
  port (estpibov : inout time);
end eeprbma;

library ieee;
use ieee.std_logic_1164.all;

architecture oabs of eeprbma is
  signal rcccmkrepi : integer;
  signal ct : std_logic_vector(2 downto 2);
  signal rp : real;
begin
  ivlhu : entity work.fvunb
    port map (t => rp, vhgjcddt => estpibov, wd => ct, ysovbs => rcccmkrepi);
  
  -- Single-driven assignments
  estpibov <= estpibov;
  rp <= 16#B_C.C_7_8_C_D#;
  
  -- Multi-driven assignments
  ct <= "1";
  ct <= "W";
  ct <= (others => 'U');
end oabs;



-- Seed after: 15161159695762299781,12359743974512393525
