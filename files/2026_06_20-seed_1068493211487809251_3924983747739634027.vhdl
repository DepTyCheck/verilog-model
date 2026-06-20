-- Seed: 1068493211487809251,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity qpaiaiupm is
  port (pnxrjc : out std_logic; hrrwstizxj : out std_logic_vector(4 downto 3));
end qpaiaiupm;

architecture kjjvmpith of qpaiaiupm is
  
begin
  -- Multi-driven assignments
  hrrwstizxj <= ('-', 'L');
end kjjvmpith;

entity tpkfsykrsg is
  port (nltaysi : inout real);
end tpkfsykrsg;

library ieee;
use ieee.std_logic_1164.all;

architecture bdfbvqjms of tpkfsykrsg is
  signal aprr : std_logic;
  signal tlbq : std_logic_vector(4 downto 3);
  signal ocinm : std_logic;
  signal mqsjtai : std_logic_vector(4 downto 3);
  signal cdwyfxym : std_logic;
begin
  ebbpuug : entity work.qpaiaiupm
    port map (pnxrjc => cdwyfxym, hrrwstizxj => mqsjtai);
  aaz : entity work.qpaiaiupm
    port map (pnxrjc => ocinm, hrrwstizxj => tlbq);
  gc : entity work.qpaiaiupm
    port map (pnxrjc => aprr, hrrwstizxj => tlbq);
  
  -- Single-driven assignments
  nltaysi <= 2#1_1_1_0_1.01001#;
  
  -- Multi-driven assignments
  mqsjtai <= ('L', 'L');
  cdwyfxym <= 'W';
  aprr <= 'X';
  mqsjtai <= ('H', 'L');
end bdfbvqjms;

entity h is
  port (knjva : out bit);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture theug of h is
  signal jjmedtay : std_logic;
  signal fmjogmtwxu : std_logic_vector(4 downto 3);
  signal tchnrx : std_logic;
begin
  cura : entity work.qpaiaiupm
    port map (pnxrjc => tchnrx, hrrwstizxj => fmjogmtwxu);
  os : entity work.qpaiaiupm
    port map (pnxrjc => jjmedtay, hrrwstizxj => fmjogmtwxu);
  
  -- Single-driven assignments
  knjva <= '1';
  
  -- Multi-driven assignments
  tchnrx <= 'U';
  tchnrx <= 'U';
end theug;

library ieee;
use ieee.std_logic_1164.all;

entity vrnyunkwnt is
  port (tby : in std_logic; ywiur : linkage real);
end vrnyunkwnt;

library ieee;
use ieee.std_logic_1164.all;

architecture uasbzd of vrnyunkwnt is
  signal j : std_logic_vector(4 downto 3);
  signal ppd : std_logic;
  signal tacrh : real;
  signal elq : bit;
begin
  pwo : entity work.h
    port map (knjva => elq);
  n : entity work.tpkfsykrsg
    port map (nltaysi => tacrh);
  ioou : entity work.qpaiaiupm
    port map (pnxrjc => ppd, hrrwstizxj => j);
end uasbzd;



-- Seed after: 17780943795871528600,3924983747739634027
