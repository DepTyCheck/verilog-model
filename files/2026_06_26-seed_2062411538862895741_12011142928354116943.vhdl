-- Seed: 2062411538862895741,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity huzzalov is
  port (yx : linkage real; mu : inout time; lhd : buffer std_logic; ejbqvfmnsx : in time);
end huzzalov;

architecture mwlh of huzzalov is
  
begin
  -- Single-driven assignments
  mu <= 2#1111# us;
  
  -- Multi-driven assignments
  lhd <= 'U';
end mwlh;

entity fgaftvzqu is
  port (ggra : inout time_vector(1 downto 1); lqmbmjz : linkage time; qaraedgen : inout time_vector(4 downto 1); zmemtwj : inout time);
end fgaftvzqu;

library ieee;
use ieee.std_logic_1164.all;

architecture ueocwo of fgaftvzqu is
  signal gjum : time;
  signal gqueosva : std_logic;
  signal hx : time;
  signal tfjdl : real;
begin
  wcpmmn : entity work.huzzalov
    port map (yx => tfjdl, mu => hx, lhd => gqueosva, ejbqvfmnsx => gjum);
  
  -- Single-driven assignments
  zmemtwj <= 1 hr;
  gjum <= 0 fs;
  qaraedgen <= (2#00# us, 2#1_0_0_1_0# ms, 32 ns, 3221 ns);
  
  -- Multi-driven assignments
  gqueosva <= '-';
end ueocwo;



-- Seed after: 9975245258516510420,12011142928354116943
