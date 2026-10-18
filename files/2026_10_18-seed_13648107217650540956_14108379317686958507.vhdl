-- Seed: 13648107217650540956,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity piwso is
  port (vtlhrq : buffer std_logic; rjlqhnw : inout std_logic; gpla : out time; zqv : out std_logic);
end piwso;

architecture n of piwso is
  
begin
  -- Single-driven assignments
  gpla <= gpla;
  
  -- Multi-driven assignments
  zqv <= zqv;
end n;

library ieee;
use ieee.std_logic_1164.all;

entity ywiaqz is
  port (feownd : inout std_logic_vector(0 downto 1));
end ywiaqz;

library ieee;
use ieee.std_logic_1164.all;

architecture bspwxaya of ywiaqz is
  signal q : std_logic;
  signal uex : time;
  signal tvsafqctbg : std_logic;
  signal c : std_logic;
  signal yxx : time;
  signal capdbqd : std_logic;
begin
  epzlpdjod : entity work.piwso
    port map (vtlhrq => capdbqd, rjlqhnw => capdbqd, gpla => yxx, zqv => c);
  uuyywfzki : entity work.piwso
    port map (vtlhrq => capdbqd, rjlqhnw => tvsafqctbg, gpla => uex, zqv => q);
  
  -- Multi-driven assignments
  feownd <= "";
  feownd <= feownd;
end bspwxaya;

library ieee;
use ieee.std_logic_1164.all;

entity erhmnih is
  port (jphqxy : inout std_logic);
end erhmnih;

library ieee;
use ieee.std_logic_1164.all;

architecture syx of erhmnih is
  signal ypjufds : std_logic;
  signal ohqlzupbzd : time;
  signal rlsorl : std_logic;
  signal kateg : std_logic;
  signal egzrdqxjj : time;
  signal kepxzbtfnz : std_logic;
  signal hm : std_logic;
  signal tv : std_logic_vector(0 downto 1);
begin
  eyapfasio : entity work.ywiaqz
    port map (feownd => tv);
  ubo : entity work.ywiaqz
    port map (feownd => tv);
  r : entity work.piwso
    port map (vtlhrq => hm, rjlqhnw => kepxzbtfnz, gpla => egzrdqxjj, zqv => kateg);
  jpf : entity work.piwso
    port map (vtlhrq => rlsorl, rjlqhnw => rlsorl, gpla => ohqlzupbzd, zqv => ypjufds);
  
  -- Multi-driven assignments
  kepxzbtfnz <= jphqxy;
  hm <= 'L';
  jphqxy <= 'L';
  rlsorl <= kateg;
end syx;



-- Seed after: 6144663958820008699,14108379317686958507
