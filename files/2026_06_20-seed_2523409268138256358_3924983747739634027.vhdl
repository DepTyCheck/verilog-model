-- Seed: 2523409268138256358,3924983747739634027

entity kzwpy is
  port (lek : in bit_vector(1 downto 0));
end kzwpy;

architecture n of kzwpy is
  
begin
  
end n;

entity ddgpsumk is
  port (vggev : out time_vector(2 downto 3));
end ddgpsumk;

architecture hpcnmib of ddgpsumk is
  signal a : bit_vector(1 downto 0);
  signal mmd : bit_vector(1 downto 0);
  signal l : bit_vector(1 downto 0);
begin
  kfcnuqvg : entity work.kzwpy
    port map (lek => l);
  blydfvc : entity work.kzwpy
    port map (lek => mmd);
  taiutk : entity work.kzwpy
    port map (lek => a);
  bk : entity work.kzwpy
    port map (lek => l);
  
  -- Single-driven assignments
  a <= ('1', '0');
  mmd <= ('1', '1');
  l <= ('0', '0');
  vggev <= (others => 0 ns);
end hpcnmib;

library ieee;
use ieee.std_logic_1164.all;

entity vf is
  port (l : in boolean; tcboefvrd : in std_logic_vector(4 to 0); jmn : in std_logic_vector(2 to 4));
end vf;

architecture pj of vf is
  
begin
  
end pj;

library ieee;
use ieee.std_logic_1164.all;

entity bwmun is
  port (taxlt : in std_logic; fmuh : linkage boolean_vector(2 downto 1); omv : inout integer; kjjjayhwpz : buffer integer_vector(4 downto 2));
end bwmun;

library ieee;
use ieee.std_logic_1164.all;

architecture ajucokvrf of bwmun is
  signal dyeo : time_vector(2 downto 3);
  signal qv : std_logic_vector(2 to 4);
  signal isbanex : std_logic_vector(4 to 0);
  signal pod : boolean;
  signal thum : bit_vector(1 downto 0);
begin
  yxztymjkzf : entity work.kzwpy
    port map (lek => thum);
  nlfaawsaz : entity work.kzwpy
    port map (lek => thum);
  uzobo : entity work.vf
    port map (l => pod, tcboefvrd => isbanex, jmn => qv);
  xm : entity work.ddgpsumk
    port map (vggev => dyeo);
  
  -- Single-driven assignments
  kjjjayhwpz <= (8#6_1_0_3#, 43434, 2#11010#);
  thum <= ('1', '0');
  pod <= FALSE;
  omv <= 8#67436#;
  
  -- Multi-driven assignments
  isbanex <= "";
end ajucokvrf;



-- Seed after: 17680816248094144936,3924983747739634027
