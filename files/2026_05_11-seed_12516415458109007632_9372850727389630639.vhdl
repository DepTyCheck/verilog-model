-- Seed: 12516415458109007632,9372850727389630639



entity kpc is
  port (vmuvmyznv : inout time);
end kpc;



architecture itczsip of kpc is
  
begin
  
end itczsip;

library ieee;
use ieee.std_logic_1164.all;

entity vc is
  port (w : out std_logic);
end vc;



architecture bz of vc is
  signal yiuxm : time;
begin
  pwfkvv : entity work.kpc
    port map (vmuvmyznv => yiuxm);
end bz;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (geohbrt : in std_logic; ivjmvyohty : in std_logic; thkppepojy : buffer real);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture mip of s is
  signal qncrs : time;
  signal xort : std_logic;
  signal hu : time;
  signal xnsayxs : std_logic;
begin
  k : entity work.vc
    port map (w => xnsayxs);
  joww : entity work.kpc
    port map (vmuvmyznv => hu);
  qoyat : entity work.vc
    port map (w => xort);
  ojob : entity work.kpc
    port map (vmuvmyznv => qncrs);
end mip;

library ieee;
use ieee.std_logic_1164.all;

entity pjbzxekfy is
  port (cdw : in std_logic; llppw : buffer std_logic; jppglm : buffer std_logic; zsywbibgc : inout real);
end pjbzxekfy;



architecture d of pjbzxekfy is
  
begin
  zax : entity work.s
    port map (geohbrt => llppw, ivjmvyohty => jppglm, thkppepojy => zsywbibgc);
end d;



-- Seed after: 13166725921032187803,9372850727389630639
