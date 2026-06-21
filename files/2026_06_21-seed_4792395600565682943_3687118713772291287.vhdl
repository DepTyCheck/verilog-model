-- Seed: 4792395600565682943,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity jsoommb is
  port (jb : inout real_vector(3 downto 1); slqln : buffer std_logic_vector(1 downto 4); gifqe : buffer std_logic);
end jsoommb;

architecture xvvuolzwd of jsoommb is
  
begin
  -- Single-driven assignments
  jb <= (16#7.D#, 16#8_9_2_6_D.0F44#, 16#0_C_C_A.BC3D#);
  
  -- Multi-driven assignments
  slqln <= "";
  slqln <= (others => '0');
  slqln <= "";
  gifqe <= 'X';
end xvvuolzwd;

library ieee;
use ieee.std_logic_1164.all;

entity az is
  port (gzbjiu : buffer std_logic; sprvewzra : out time);
end az;

library ieee;
use ieee.std_logic_1164.all;

architecture sjgxrr of az is
  signal dzfxtj : real_vector(3 downto 1);
  signal fmmttuyxnt : std_logic;
  signal fi : std_logic_vector(1 downto 4);
  signal krwz : real_vector(3 downto 1);
  signal nx : std_logic_vector(1 downto 4);
  signal bknyop : real_vector(3 downto 1);
begin
  drkln : entity work.jsoommb
    port map (jb => bknyop, slqln => nx, gifqe => gzbjiu);
  qwtirhn : entity work.jsoommb
    port map (jb => krwz, slqln => fi, gifqe => fmmttuyxnt);
  kjyuzpzjo : entity work.jsoommb
    port map (jb => dzfxtj, slqln => fi, gifqe => gzbjiu);
  
  -- Single-driven assignments
  sprvewzra <= 4 min;
  
  -- Multi-driven assignments
  fi <= (others => '0');
  fmmttuyxnt <= 'W';
end sjgxrr;

entity ydgkabf is
  port (kfoscwcjku : buffer real; yulde : in bit_vector(0 to 4));
end ydgkabf;

library ieee;
use ieee.std_logic_1164.all;

architecture oktwfm of ydgkabf is
  signal tqlp : std_logic;
  signal ckmvx : std_logic_vector(1 downto 4);
  signal majkhk : real_vector(3 downto 1);
begin
  ysb : entity work.jsoommb
    port map (jb => majkhk, slqln => ckmvx, gifqe => tqlp);
  
  -- Single-driven assignments
  kfoscwcjku <= 412.0;
end oktwfm;



-- Seed after: 3342885302319763722,3687118713772291287
