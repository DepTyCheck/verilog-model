-- Seed: 8367674788706740783,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity ah is
  port (etrvkhcgl : in time_vector(3 to 2); ekiehbjw : inout std_logic_vector(1 downto 1));
end ah;

architecture lwe of ah is
  
begin
  -- Multi-driven assignments
  ekiehbjw <= (others => '1');
end lwe;

entity r is
  port (pfe : out real; c : linkage bit; aizov : in real_vector(2 downto 0));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture izwdlmhrjj of r is
  signal rof : std_logic_vector(1 downto 1);
  signal eermfrkmk : time_vector(3 to 2);
begin
  gxn : entity work.ah
    port map (etrvkhcgl => eermfrkmk, ekiehbjw => rof);
  
  -- Single-driven assignments
  pfe <= pfe;
  eermfrkmk <= eermfrkmk;
  
  -- Multi-driven assignments
  rof <= rof;
  rof <= rof;
end izwdlmhrjj;

library ieee;
use ieee.std_logic_1164.all;

entity ijmhq is
  port (bvx : linkage std_logic; bxbsxvn : inout integer);
end ijmhq;

library ieee;
use ieee.std_logic_1164.all;

architecture hfauxwinmt of ijmhq is
  signal lu : real_vector(2 downto 0);
  signal uwgjoz : bit;
  signal xygz : real;
  signal cswgeock : bit;
  signal sjaubtx : real;
  signal vfqj : real_vector(2 downto 0);
  signal xfgoqz : bit;
  signal udoif : real;
  signal l : std_logic_vector(1 downto 1);
  signal ozu : time_vector(3 to 2);
begin
  iosdkkm : entity work.ah
    port map (etrvkhcgl => ozu, ekiehbjw => l);
  cttxyrubtt : entity work.r
    port map (pfe => udoif, c => xfgoqz, aizov => vfqj);
  u : entity work.r
    port map (pfe => sjaubtx, c => cswgeock, aizov => vfqj);
  ugdv : entity work.r
    port map (pfe => xygz, c => uwgjoz, aizov => lu);
  
  -- Single-driven assignments
  lu <= vfqj;
  ozu <= (others => 0 ns);
  bxbsxvn <= bxbsxvn;
end hfauxwinmt;



-- Seed after: 9044525113553709345,6562834012285671529
