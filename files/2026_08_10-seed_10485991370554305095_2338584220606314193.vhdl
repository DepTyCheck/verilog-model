-- Seed: 10485991370554305095,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity jfj is
  port (dg : inout time; nag : out std_logic_vector(2 to 3); onkkcnovh : in std_logic_vector(1 to 2));
end jfj;

architecture lmaryqmb of jfj is
  
begin
  -- Multi-driven assignments
  nag <= nag;
  nag <= nag;
  nag <= onkkcnovh;
  nag <= "H0";
end lmaryqmb;

entity jsuh is
  port (xjclgbpqe : inout time; rlqlljyfz : inout time);
end jsuh;

library ieee;
use ieee.std_logic_1164.all;

architecture gjldghvf of jsuh is
  signal vbz : std_logic_vector(1 to 2);
  signal eakblnehlh : std_logic_vector(2 to 3);
  signal bcthi : std_logic_vector(2 to 3);
  signal oz : time;
begin
  afnywesm : entity work.jfj
    port map (dg => oz, nag => bcthi, onkkcnovh => eakblnehlh);
  ddglkgdr : entity work.jfj
    port map (dg => rlqlljyfz, nag => eakblnehlh, onkkcnovh => bcthi);
  ulzkvti : entity work.jfj
    port map (dg => xjclgbpqe, nag => bcthi, onkkcnovh => vbz);
  
  -- Multi-driven assignments
  eakblnehlh <= "LL";
  bcthi <= eakblnehlh;
end gjldghvf;



-- Seed after: 5369386282895706171,2338584220606314193
