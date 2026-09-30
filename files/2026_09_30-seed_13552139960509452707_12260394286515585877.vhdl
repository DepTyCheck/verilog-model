-- Seed: 13552139960509452707,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity rmwyl is
  port (dgt : out real; bdfhcadug : out std_logic_vector(3 to 1); mxg : buffer time_vector(4 downto 4); qankds : buffer integer);
end rmwyl;

architecture csuj of rmwyl is
  
begin
  -- Multi-driven assignments
  bdfhcadug <= "";
  bdfhcadug <= bdfhcadug;
  bdfhcadug <= (others => '0');
end csuj;

library ieee;
use ieee.std_logic_1164.all;

entity pyt is
  port (lcv : in time; rwkjwxlch : out real; sz : out std_logic);
end pyt;

library ieee;
use ieee.std_logic_1164.all;

architecture vrjlfy of pyt is
  signal vmnghxa : integer;
  signal jpttd : time_vector(4 downto 4);
  signal jhwnymcel : std_logic_vector(3 to 1);
  signal vdjlsdoch : real;
  signal qccdog : integer;
  signal pxsior : time_vector(4 downto 4);
  signal idqcjxq : integer;
  signal d : time_vector(4 downto 4);
  signal njvrx : std_logic_vector(3 to 1);
  signal aver : real;
begin
  prkixub : entity work.rmwyl
    port map (dgt => aver, bdfhcadug => njvrx, mxg => d, qankds => idqcjxq);
  hpmksr : entity work.rmwyl
    port map (dgt => rwkjwxlch, bdfhcadug => njvrx, mxg => pxsior, qankds => qccdog);
  fe : entity work.rmwyl
    port map (dgt => vdjlsdoch, bdfhcadug => jhwnymcel, mxg => jpttd, qankds => vmnghxa);
  
  -- Multi-driven assignments
  jhwnymcel <= "";
  sz <= 'X';
  sz <= 'Z';
  sz <= '1';
end vrjlfy;

library ieee;
use ieee.std_logic_1164.all;

entity ahiaveu is
  port (qnryxox : in std_logic_vector(2 to 2); nzvp : linkage std_logic_vector(4 to 0));
end ahiaveu;

library ieee;
use ieee.std_logic_1164.all;

architecture rti of ahiaveu is
  signal dgflzc : integer;
  signal nc : time_vector(4 downto 4);
  signal qnnry : std_logic_vector(3 to 1);
  signal wsxhfzmefq : real;
  signal kms : std_logic;
  signal j : real;
  signal qvvhsifpp : time;
begin
  mun : entity work.pyt
    port map (lcv => qvvhsifpp, rwkjwxlch => j, sz => kms);
  nen : entity work.rmwyl
    port map (dgt => wsxhfzmefq, bdfhcadug => qnnry, mxg => nc, qankds => dgflzc);
  
  -- Single-driven assignments
  qvvhsifpp <= 0_4_0.3 fs;
  
  -- Multi-driven assignments
  kms <= kms;
  kms <= 'Z';
  kms <= kms;
end rti;

library ieee;
use ieee.std_logic_1164.all;

entity owdaaipija is
  port (rlx : inout time; m : in real_vector(2 downto 0); uawkdq : linkage boolean_vector(3 to 2); lrudc : out std_logic);
end owdaaipija;

library ieee;
use ieee.std_logic_1164.all;

architecture t of owdaaipija is
  signal de : std_logic;
  signal jm : real;
  signal wcuikcym : integer;
  signal otjvwmruc : time_vector(4 downto 4);
  signal gke : std_logic_vector(3 to 1);
  signal yp : real;
begin
  nxarbyrm : entity work.rmwyl
    port map (dgt => yp, bdfhcadug => gke, mxg => otjvwmruc, qankds => wcuikcym);
  aqwwrtch : entity work.pyt
    port map (lcv => rlx, rwkjwxlch => jm, sz => de);
  
  -- Multi-driven assignments
  lrudc <= lrudc;
  lrudc <= lrudc;
  de <= 'Z';
  de <= lrudc;
end t;



-- Seed after: 12498451620611475836,12260394286515585877
