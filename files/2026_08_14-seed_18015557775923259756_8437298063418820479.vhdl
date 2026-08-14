-- Seed: 18015557775923259756,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (bfat : inout std_logic_vector(0 to 4); ajocexn : inout std_logic);
end z;

architecture hmecrk of z is
  
begin
  -- Multi-driven assignments
  ajocexn <= ajocexn;
  bfat <= bfat;
  ajocexn <= 'W';
  ajocexn <= 'L';
end hmecrk;

entity wl is
  port (mauj : out real);
end wl;

library ieee;
use ieee.std_logic_1164.all;

architecture maov of wl is
  signal ndwais : std_logic;
  signal ohuj : std_logic_vector(0 to 4);
  signal bgzhsxfg : std_logic_vector(0 to 4);
  signal ixmk : std_logic_vector(0 to 4);
  signal qq : std_logic;
  signal lfar : std_logic_vector(0 to 4);
begin
  utvegufn : entity work.z
    port map (bfat => lfar, ajocexn => qq);
  bc : entity work.z
    port map (bfat => ixmk, ajocexn => qq);
  pvrpfgnmhc : entity work.z
    port map (bfat => bgzhsxfg, ajocexn => qq);
  j : entity work.z
    port map (bfat => ohuj, ajocexn => ndwais);
  
  -- Single-driven assignments
  mauj <= 16#8_F_9_7_3.8_3_F_B_D#;
  
  -- Multi-driven assignments
  ndwais <= 'X';
  ixmk <= ohuj;
  bgzhsxfg <= ('1', 'W', 'X', 'Z', 'X');
  lfar <= ixmk;
end maov;



-- Seed after: 9451957234068532688,8437298063418820479
