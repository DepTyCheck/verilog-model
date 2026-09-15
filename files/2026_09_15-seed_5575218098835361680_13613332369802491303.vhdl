-- Seed: 5575218098835361680,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (migia : inout std_logic; akchcruwvh : buffer std_logic_vector(2 downto 0); ntlynaim : buffer real_vector(1 to 2));
end u;

architecture bvhadl of u is
  
begin
  
end bvhadl;

library ieee;
use ieee.std_logic_1164.all;

entity xbhpr is
  port (cwcdkk : out time; tib : out std_logic);
end xbhpr;

library ieee;
use ieee.std_logic_1164.all;

architecture thsiw of xbhpr is
  signal gt : real_vector(1 to 2);
  signal larjwt : std_logic_vector(2 downto 0);
  signal rrovubcoq : std_logic;
begin
  ngprmyjwu : entity work.u
    port map (migia => rrovubcoq, akchcruwvh => larjwt, ntlynaim => gt);
  
  -- Single-driven assignments
  cwcdkk <= 1 hr;
  
  -- Multi-driven assignments
  tib <= 'L';
end thsiw;

library ieee;
use ieee.std_logic_1164.all;

entity epvedz is
  port (gdzyyudah : buffer std_logic_vector(1 to 0));
end epvedz;

library ieee;
use ieee.std_logic_1164.all;

architecture lkanielxk of epvedz is
  signal w : real_vector(1 to 2);
  signal laib : std_logic_vector(2 downto 0);
  signal q : std_logic;
  signal fwf : real_vector(1 to 2);
  signal vp : std_logic_vector(2 downto 0);
  signal r : std_logic;
begin
  ux : entity work.u
    port map (migia => r, akchcruwvh => vp, ntlynaim => fwf);
  b : entity work.u
    port map (migia => q, akchcruwvh => laib, ntlynaim => w);
  
  -- Multi-driven assignments
  gdzyyudah <= gdzyyudah;
  laib <= ('Z', 'W', '-');
end lkanielxk;



-- Seed after: 2465634386814111871,13613332369802491303
