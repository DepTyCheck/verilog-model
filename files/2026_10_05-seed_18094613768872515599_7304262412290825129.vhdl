-- Seed: 18094613768872515599,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity uxprd is
  port (alo : in std_logic; bzel : in real; zegs : linkage time_vector(4 downto 1));
end uxprd;

architecture pxaghg of uxprd is
  
begin
  
end pxaghg;

entity mhpqdatkwg is
  port (pgptlnds : inout real);
end mhpqdatkwg;

library ieee;
use ieee.std_logic_1164.all;

architecture lnioh of mhpqdatkwg is
  signal ztdswr : time_vector(4 downto 1);
  signal yealimfmei : std_logic;
  signal e : time_vector(4 downto 1);
  signal xqdm : time_vector(4 downto 1);
  signal ydivgo : std_logic;
begin
  yhuaohkbhz : entity work.uxprd
    port map (alo => ydivgo, bzel => pgptlnds, zegs => xqdm);
  nz : entity work.uxprd
    port map (alo => ydivgo, bzel => pgptlnds, zegs => e);
  jehfvvkz : entity work.uxprd
    port map (alo => yealimfmei, bzel => pgptlnds, zegs => ztdswr);
  
  -- Single-driven assignments
  pgptlnds <= 3_4_4.00;
end lnioh;



-- Seed after: 10544448867395317863,7304262412290825129
