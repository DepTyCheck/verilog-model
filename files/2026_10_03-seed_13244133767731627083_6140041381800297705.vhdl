-- Seed: 13244133767731627083,6140041381800297705

entity en is
  port (jbcdplz : out time; eg : inout severity_level);
end en;

architecture osqspbopnv of en is
  
begin
  
end osqspbopnv;

library ieee;
use ieee.std_logic_1164.all;

entity bddmazysk is
  port (d : in severity_level; pctnb : out std_logic_vector(4 downto 4); mwj : buffer std_logic_vector(2 downto 1); vgf : inout real);
end bddmazysk;

architecture ftjuoj of bddmazysk is
  signal pxsfcdfhxq : severity_level;
  signal ov : time;
  signal xnmvyfexp : severity_level;
  signal mhefygap : time;
begin
  drh : entity work.en
    port map (jbcdplz => mhefygap, eg => xnmvyfexp);
  dr : entity work.en
    port map (jbcdplz => ov, eg => pxsfcdfhxq);
  
  -- Single-driven assignments
  vgf <= vgf;
  
  -- Multi-driven assignments
  pctnb <= "L";
end ftjuoj;

library ieee;
use ieee.std_logic_1164.all;

entity zmkuyyk is
  port (jiovmtsq : out std_logic_vector(0 to 3); la : linkage real; zymcu : out integer; cc : in integer);
end zmkuyyk;

architecture vpxahe of zmkuyyk is
  signal aowhpqgr : severity_level;
  signal dpgqv : time;
begin
  j : entity work.en
    port map (jbcdplz => dpgqv, eg => aowhpqgr);
  
  -- Single-driven assignments
  zymcu <= 2_3_3_0;
  
  -- Multi-driven assignments
  jiovmtsq <= ('W', 'W', 'X', 'W');
  jiovmtsq <= jiovmtsq;
end vpxahe;

library ieee;
use ieee.std_logic_1164.all;

entity mrkuh is
  port (umgnq : linkage std_logic; hx : buffer severity_level; fn : inout integer);
end mrkuh;

library ieee;
use ieee.std_logic_1164.all;

architecture sxy of mrkuh is
  signal lnbpokw : integer;
  signal gsv : real;
  signal sj : std_logic_vector(0 to 3);
  signal edmqblh : severity_level;
  signal qmrafg : time;
  signal pkymrvu : real;
  signal d : std_logic_vector(2 downto 1);
  signal vfzxhnze : std_logic_vector(4 downto 4);
begin
  vb : entity work.bddmazysk
    port map (d => hx, pctnb => vfzxhnze, mwj => d, vgf => pkymrvu);
  sfsym : entity work.en
    port map (jbcdplz => qmrafg, eg => edmqblh);
  gimecn : entity work.zmkuyyk
    port map (jiovmtsq => sj, la => gsv, zymcu => fn, cc => lnbpokw);
  
  -- Single-driven assignments
  hx <= NOTE;
  lnbpokw <= 3_0_2;
  
  -- Multi-driven assignments
  vfzxhnze <= vfzxhnze;
  vfzxhnze <= "L";
end sxy;



-- Seed after: 12459528812517556220,6140041381800297705
