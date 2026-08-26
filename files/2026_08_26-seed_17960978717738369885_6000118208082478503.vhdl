-- Seed: 17960978717738369885,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity amii is
  port (zxaayv : linkage std_logic_vector(0 to 1); koz : linkage boolean_vector(2 to 0));
end amii;

architecture nostt of amii is
  
begin
  
end nostt;

entity k is
  port (xeviu : linkage time; hks : inout integer);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture y of k is
  signal iecaxey : boolean_vector(2 to 0);
  signal dptwvsytf : std_logic_vector(0 to 1);
  signal d : boolean_vector(2 to 0);
  signal q : boolean_vector(2 to 0);
  signal npye : boolean_vector(2 to 0);
  signal n : std_logic_vector(0 to 1);
begin
  ofptvge : entity work.amii
    port map (zxaayv => n, koz => npye);
  lfemupuj : entity work.amii
    port map (zxaayv => n, koz => q);
  g : entity work.amii
    port map (zxaayv => n, koz => d);
  iitg : entity work.amii
    port map (zxaayv => dptwvsytf, koz => iecaxey);
  
  -- Single-driven assignments
  hks <= 16#71471#;
  
  -- Multi-driven assignments
  n <= dptwvsytf;
  dptwvsytf <= n;
end y;

library ieee;
use ieee.std_logic_1164.all;

entity lp is
  port (zkyoeoxhw : in real; rgjxdz : out time; khkyubm : out integer; zcd : in std_logic);
end lp;

library ieee;
use ieee.std_logic_1164.all;

architecture wte of lp is
  signal zpkrjiu : boolean_vector(2 to 0);
  signal uqmnogzvq : std_logic_vector(0 to 1);
  signal mimurx : integer;
  signal vnmlaxj : time;
begin
  suzrcpf : entity work.k
    port map (xeviu => vnmlaxj, hks => khkyubm);
  zxnirxdfbd : entity work.k
    port map (xeviu => rgjxdz, hks => mimurx);
  fhnah : entity work.amii
    port map (zxaayv => uqmnogzvq, koz => zpkrjiu);
  
  -- Multi-driven assignments
  uqmnogzvq <= uqmnogzvq;
  uqmnogzvq <= ('Z', '1');
  uqmnogzvq <= uqmnogzvq;
  uqmnogzvq <= "ZL";
end wte;



-- Seed after: 12825604712971194931,6000118208082478503
