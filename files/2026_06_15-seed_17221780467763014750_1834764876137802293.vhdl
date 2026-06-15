-- Seed: 17221780467763014750,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity bsb is
  port (mgtnbp : linkage std_logic; dqzobk : in std_logic; pxm : buffer std_logic_vector(4 to 0); bl : out real);
end bsb;

architecture jffoksa of bsb is
  
begin
  -- Single-driven assignments
  bl <= 2_4_1.232;
  
  -- Multi-driven assignments
  pxm <= (others => '0');
  pxm <= "";
  pxm <= (others => '0');
end jffoksa;

entity xtxfi is
  port (ags : buffer real; ofo : linkage boolean; kftnsoywbo : inout integer);
end xtxfi;

library ieee;
use ieee.std_logic_1164.all;

architecture ppl of xtxfi is
  signal ytsnkgxa : real;
  signal hjzwttttrw : std_logic_vector(4 to 0);
  signal wojxwaj : real;
  signal crzimrqs : std_logic_vector(4 to 0);
  signal ohwpbxiuyn : std_logic;
  signal xafkzkkp : std_logic;
  signal vlyn : std_logic;
  signal tx : real;
  signal kywe : std_logic_vector(4 to 0);
  signal vo : std_logic;
begin
  gzmpytgiq : entity work.bsb
    port map (mgtnbp => vo, dqzobk => vo, pxm => kywe, bl => tx);
  ylxddktiet : entity work.bsb
    port map (mgtnbp => vlyn, dqzobk => vlyn, pxm => kywe, bl => ags);
  kvvzyp : entity work.bsb
    port map (mgtnbp => xafkzkkp, dqzobk => ohwpbxiuyn, pxm => crzimrqs, bl => wojxwaj);
  hlb : entity work.bsb
    port map (mgtnbp => vlyn, dqzobk => vo, pxm => hjzwttttrw, bl => ytsnkgxa);
  
  -- Multi-driven assignments
  ohwpbxiuyn <= '1';
  xafkzkkp <= 'L';
  vlyn <= 'H';
  vo <= '-';
end ppl;



-- Seed after: 4840882974306912454,1834764876137802293
