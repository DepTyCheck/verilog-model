-- Seed: 3719395828534529319,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity vbff is
  port (k : in std_logic_vector(1 to 0); igly : inout time; arr : in std_logic; wfog : inout real);
end vbff;



architecture daluz of vbff is
  
begin
  
end daluz;



entity rqncou is
  port (tsifh : out time; nylafr : out integer);
end rqncou;

library ieee;
use ieee.std_logic_1164.all;

architecture bynrbp of rqncou is
  signal fwuj : real;
  signal i : std_logic;
  signal oihnplxv : time;
  signal pbpmszp : real;
  signal z : std_logic;
  signal hg : time;
  signal hdfqwn : real;
  signal ocz : std_logic;
  signal sct : std_logic_vector(1 to 0);
begin
  unvyrrkvmm : entity work.vbff
    port map (k => sct, igly => tsifh, arr => ocz, wfog => hdfqwn);
  agcwdyd : entity work.vbff
    port map (k => sct, igly => hg, arr => z, wfog => pbpmszp);
  yclejyb : entity work.vbff
    port map (k => sct, igly => oihnplxv, arr => i, wfog => fwuj);
end bynrbp;



entity srwlzod is
  port (miolvn : out integer; wp : in character);
end srwlzod;

library ieee;
use ieee.std_logic_1164.all;

architecture mtaxyxym of srwlzod is
  signal macwzbo : real;
  signal p : time;
  signal gelk : std_logic_vector(1 to 0);
  signal ecbmew : time;
  signal nxanoqw : real;
  signal fz : std_logic;
  signal dluvayir : time;
  signal ujvaa : std_logic_vector(1 to 0);
begin
  gfbwpcf : entity work.vbff
    port map (k => ujvaa, igly => dluvayir, arr => fz, wfog => nxanoqw);
  yuj : entity work.rqncou
    port map (tsifh => ecbmew, nylafr => miolvn);
  ff : entity work.vbff
    port map (k => gelk, igly => p, arr => fz, wfog => macwzbo);
end mtaxyxym;



entity kuzoaukouf is
  port (rufhzlkp : in severity_level; iksroxt : inout real; plv : out time; nzasfxz : inout boolean);
end kuzoaukouf;



architecture ysvscqfkz of kuzoaukouf is
  signal sgag : integer;
begin
  emxkmzm : entity work.rqncou
    port map (tsifh => plv, nylafr => sgag);
end ysvscqfkz;



-- Seed after: 16405498551780507608,7332793847894666635
