-- Seed: 8436974127105477873,13843488114570579517

entity phj is
  port (hfmv : inout integer; nnpw : in integer);
end phj;

architecture w of phj is
  
begin
  -- Single-driven assignments
  hfmv <= nnpw;
end w;

entity a is
  port (jipvmeu : inout integer; ontv : linkage time);
end a;

architecture ys of a is
  signal qflldvy : integer;
  signal qoxdyfmaf : integer;
begin
  idzgg : entity work.phj
    port map (hfmv => qoxdyfmaf, nnpw => jipvmeu);
  hxrkxddxyq : entity work.phj
    port map (hfmv => jipvmeu, nnpw => jipvmeu);
  oloxmk : entity work.phj
    port map (hfmv => qflldvy, nnpw => jipvmeu);
end ys;

library ieee;
use ieee.std_logic_1164.all;

entity cgwfowwp is
  port (jyjauzqkpi : buffer std_logic_vector(2 to 3); caufluli : inout integer; pakpwjx : linkage std_logic; qpp : in real);
end cgwfowwp;

architecture z of cgwfowwp is
  signal ftwqlf : integer;
  signal jyoui : integer;
  signal qntkd : integer;
  signal kgsj : integer;
begin
  u : entity work.phj
    port map (hfmv => kgsj, nnpw => caufluli);
  hn : entity work.phj
    port map (hfmv => qntkd, nnpw => qntkd);
  g : entity work.phj
    port map (hfmv => caufluli, nnpw => jyoui);
  tsz : entity work.phj
    port map (hfmv => jyoui, nnpw => ftwqlf);
  
  -- Single-driven assignments
  ftwqlf <= 3;
  
  -- Multi-driven assignments
  jyjauzqkpi <= ('L', 'Z');
  jyjauzqkpi <= jyjauzqkpi;
  jyjauzqkpi <= ('H', 'X');
end z;



-- Seed after: 14553204881519869182,13843488114570579517
