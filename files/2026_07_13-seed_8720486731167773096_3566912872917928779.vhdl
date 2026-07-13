-- Seed: 8720486731167773096,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity urxhrshnd is
  port (e : linkage real; qhb : out std_logic_vector(3 downto 4); dc : out bit);
end urxhrshnd;

architecture iodorzk of urxhrshnd is
  
begin
  -- Single-driven assignments
  dc <= '1';
end iodorzk;

library ieee;
use ieee.std_logic_1164.all;

entity dllbbqgo is
  port (osb : buffer std_logic_vector(4 downto 1); a : out bit);
end dllbbqgo;

library ieee;
use ieee.std_logic_1164.all;

architecture xvcwfi of dllbbqgo is
  signal v : bit;
  signal idk : real;
  signal rjif : std_logic_vector(3 downto 4);
  signal tmybdxxlja : real;
  signal rzuhmjxcx : bit;
  signal hqvc : std_logic_vector(3 downto 4);
  signal recjahvjxl : real;
  signal matju : bit;
  signal uwz : std_logic_vector(3 downto 4);
  signal xugwmhq : real;
begin
  t : entity work.urxhrshnd
    port map (e => xugwmhq, qhb => uwz, dc => matju);
  dhmtqyj : entity work.urxhrshnd
    port map (e => recjahvjxl, qhb => hqvc, dc => rzuhmjxcx);
  pmcy : entity work.urxhrshnd
    port map (e => tmybdxxlja, qhb => rjif, dc => a);
  jelljzhr : entity work.urxhrshnd
    port map (e => idk, qhb => uwz, dc => v);
  
  -- Multi-driven assignments
  uwz <= (others => '0');
  osb <= osb;
  osb <= "Z10U";
end xvcwfi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wurlikku is
  port (zpkgto : in std_logic_vector(2 to 2); ccalnsbmo : inout array_value_mirror; dqppzdhnis : linkage real);
end wurlikku;

library ieee;
use ieee.std_logic_1164.all;

architecture f of wurlikku is
  signal tbmukwii : bit;
  signal whazu : std_logic_vector(4 downto 1);
  signal wzg : bit;
  signal pekfa : real;
  signal pzbmdjcdoq : bit;
  signal byowstwmw : std_logic_vector(3 downto 4);
  signal mauzcoo : bit;
  signal yqyzryz : std_logic_vector(3 downto 4);
  signal cwaxq : real;
begin
  mplshcx : entity work.urxhrshnd
    port map (e => cwaxq, qhb => yqyzryz, dc => mauzcoo);
  igdimtz : entity work.urxhrshnd
    port map (e => dqppzdhnis, qhb => byowstwmw, dc => pzbmdjcdoq);
  dobcvp : entity work.urxhrshnd
    port map (e => pekfa, qhb => yqyzryz, dc => wzg);
  ksfylel : entity work.dllbbqgo
    port map (osb => whazu, a => tbmukwii);
  
  -- Multi-driven assignments
  yqyzryz <= byowstwmw;
end f;



-- Seed after: 8460158664610750530,3566912872917928779
