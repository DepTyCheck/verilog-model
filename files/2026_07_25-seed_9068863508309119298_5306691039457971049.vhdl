-- Seed: 9068863508309119298,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (xbgj : inout time; zlmijxhdhk : inout std_logic_vector(4 downto 0); oxlpagrb : in std_logic_vector(2 to 1));
end p;

architecture tdgyryafk of p is
  
begin
  -- Single-driven assignments
  xbgj <= xbgj;
  
  -- Multi-driven assignments
  zlmijxhdhk <= ('X', '-', 'L', '-', 'X');
  zlmijxhdhk <= ('L', '0', '1', 'U', '-');
end tdgyryafk;

entity qxkzt is
  port (kssdrlzvoh : inout real; gwdlzll : linkage string(4 downto 2); xygdvm : inout integer);
end qxkzt;

library ieee;
use ieee.std_logic_1164.all;

architecture genyoo of qxkzt is
  signal rmdnueq : time;
  signal xs : std_logic_vector(2 to 1);
  signal jvlcw : std_logic_vector(4 downto 0);
  signal b : time;
begin
  wwxogicx : entity work.p
    port map (xbgj => b, zlmijxhdhk => jvlcw, oxlpagrb => xs);
  r : entity work.p
    port map (xbgj => rmdnueq, zlmijxhdhk => jvlcw, oxlpagrb => xs);
end genyoo;

library ieee;
use ieee.std_logic_1164.all;

entity orxkew is
  port (t : in std_logic; tzbt : in severity_level; by : inout time);
end orxkew;

library ieee;
use ieee.std_logic_1164.all;

architecture xdinx of orxkew is
  signal ohbot : std_logic_vector(2 to 1);
  signal gbgwrlvhof : std_logic_vector(2 to 1);
  signal qkgdr : time;
  signal upfahe : integer;
  signal xtgk : string(4 downto 2);
  signal fq : real;
  signal axqd : std_logic_vector(2 to 1);
  signal fl : std_logic_vector(4 downto 0);
  signal pexydm : time;
begin
  qx : entity work.p
    port map (xbgj => pexydm, zlmijxhdhk => fl, oxlpagrb => axqd);
  ijgocqggj : entity work.qxkzt
    port map (kssdrlzvoh => fq, gwdlzll => xtgk, xygdvm => upfahe);
  gbupwxeer : entity work.p
    port map (xbgj => qkgdr, zlmijxhdhk => fl, oxlpagrb => gbgwrlvhof);
  dgyhvdzj : entity work.p
    port map (xbgj => by, zlmijxhdhk => fl, oxlpagrb => ohbot);
  
  -- Multi-driven assignments
  ohbot <= gbgwrlvhof;
end xdinx;

entity ntd is
  port (tfk : out real);
end ntd;

library ieee;
use ieee.std_logic_1164.all;

architecture vlkxse of ntd is
  signal cuxlpmjs : time;
  signal qb : severity_level;
  signal ozijwa : std_logic;
  signal bfpmhmq : time;
  signal mghvkekxv : severity_level;
  signal jcl : std_logic;
begin
  vfg : entity work.orxkew
    port map (t => jcl, tzbt => mghvkekxv, by => bfpmhmq);
  enwnzf : entity work.orxkew
    port map (t => ozijwa, tzbt => qb, by => cuxlpmjs);
  
  -- Single-driven assignments
  tfk <= tfk;
  mghvkekxv <= NOTE;
  
  -- Multi-driven assignments
  jcl <= jcl;
  jcl <= jcl;
  jcl <= jcl;
end vlkxse;



-- Seed after: 17275350142067381283,5306691039457971049
