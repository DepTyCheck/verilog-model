-- Seed: 17425499562735265763,7198033922882419595

entity fxsdyxjqrn is
  port (ctew : inout real);
end fxsdyxjqrn;

architecture clk of fxsdyxjqrn is
  
begin
  -- Single-driven assignments
  ctew <= 0_0_0.04;
end clk;

library ieee;
use ieee.std_logic_1164.all;

entity eyczbzld is
  port (tgwf : inout time; vfaxam : out std_logic_vector(2 downto 1));
end eyczbzld;

architecture ekdvg of eyczbzld is
  signal ohl : real;
  signal xkegbzp : real;
begin
  v : entity work.fxsdyxjqrn
    port map (ctew => xkegbzp);
  znomlwta : entity work.fxsdyxjqrn
    port map (ctew => ohl);
  
  -- Single-driven assignments
  tgwf <= 341 ms;
  
  -- Multi-driven assignments
  vfaxam <= vfaxam;
  vfaxam <= ('Z', 'Z');
  vfaxam <= vfaxam;
end ekdvg;



-- Seed after: 6044451278115816908,7198033922882419595
