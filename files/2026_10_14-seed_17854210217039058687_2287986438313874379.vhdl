-- Seed: 17854210217039058687,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity nifeputjys is
  port (eilsfuqnyj : buffer std_logic_vector(3 to 3); hwqbmgdsu : inout integer; qhabdcna : in time; jydiwnhf : inout time);
end nifeputjys;

architecture ifhmz of nifeputjys is
  
begin
  -- Single-driven assignments
  jydiwnhf <= 2#001.0# fs;
  hwqbmgdsu <= hwqbmgdsu;
  
  -- Multi-driven assignments
  eilsfuqnyj <= (others => 'H');
end ifhmz;

entity n is
  port (tnvwypa : linkage time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture uvhteijj of n is
  signal g : time;
  signal rtdgm : integer;
  signal bgsfnuve : integer;
  signal jzneuis : std_logic_vector(3 to 3);
  signal jnpkvcwgm : time;
  signal jvy : integer;
  signal dctkbz : time;
  signal pwka : time;
  signal qteildon : integer;
  signal bufv : std_logic_vector(3 to 3);
begin
  mcv : entity work.nifeputjys
    port map (eilsfuqnyj => bufv, hwqbmgdsu => qteildon, qhabdcna => pwka, jydiwnhf => dctkbz);
  xirizi : entity work.nifeputjys
    port map (eilsfuqnyj => bufv, hwqbmgdsu => jvy, qhabdcna => jnpkvcwgm, jydiwnhf => pwka);
  cwcojwzp : entity work.nifeputjys
    port map (eilsfuqnyj => jzneuis, hwqbmgdsu => bgsfnuve, qhabdcna => pwka, jydiwnhf => jnpkvcwgm);
  mzh : entity work.nifeputjys
    port map (eilsfuqnyj => bufv, hwqbmgdsu => rtdgm, qhabdcna => g, jydiwnhf => g);
  
  -- Multi-driven assignments
  bufv <= (others => 'X');
end uvhteijj;



-- Seed after: 16369610413922790558,2287986438313874379
