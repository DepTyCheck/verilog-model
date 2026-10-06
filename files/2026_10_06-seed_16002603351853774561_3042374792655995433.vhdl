-- Seed: 16002603351853774561,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity cggbfx is
  port (aimzc : in real; guoilgcxf : out std_logic_vector(4 to 3));
end cggbfx;

architecture j of cggbfx is
  
begin
  -- Multi-driven assignments
  guoilgcxf <= "";
  guoilgcxf <= guoilgcxf;
  guoilgcxf <= guoilgcxf;
  guoilgcxf <= (others => '0');
end j;

entity efkgq is
  port (wsgwlhk : linkage time_vector(3 to 1); bawnjj : buffer boolean; k : in severity_level);
end efkgq;

library ieee;
use ieee.std_logic_1164.all;

architecture zp of efkgq is
  signal b : real;
  signal xcqugcnyj : std_logic_vector(4 to 3);
  signal jg : real;
  signal tujv : std_logic_vector(4 to 3);
  signal sfhhu : real;
begin
  ct : entity work.cggbfx
    port map (aimzc => sfhhu, guoilgcxf => tujv);
  nyeo : entity work.cggbfx
    port map (aimzc => jg, guoilgcxf => xcqugcnyj);
  fqvxgqto : entity work.cggbfx
    port map (aimzc => b, guoilgcxf => tujv);
  
  -- Single-driven assignments
  bawnjj <= TRUE;
  
  -- Multi-driven assignments
  xcqugcnyj <= "";
  tujv <= "";
  xcqugcnyj <= xcqugcnyj;
  tujv <= tujv;
end zp;

library ieee;
use ieee.std_logic_1164.all;

entity vaujdn is
  port (pxtdumcu : linkage std_logic);
end vaujdn;

architecture mlouogs of vaujdn is
  signal shvavqhozv : severity_level;
  signal tjrzaz : boolean;
  signal jdpqr : time_vector(3 to 1);
  signal rvlwhbpbh : severity_level;
  signal yxbaxww : boolean;
  signal ijsbl : time_vector(3 to 1);
begin
  ehuwovnexr : entity work.efkgq
    port map (wsgwlhk => ijsbl, bawnjj => yxbaxww, k => rvlwhbpbh);
  oxrtadaeb : entity work.efkgq
    port map (wsgwlhk => jdpqr, bawnjj => tjrzaz, k => shvavqhozv);
  
  -- Single-driven assignments
  rvlwhbpbh <= rvlwhbpbh;
  shvavqhozv <= WARNING;
end mlouogs;



-- Seed after: 3312544537915872691,3042374792655995433
