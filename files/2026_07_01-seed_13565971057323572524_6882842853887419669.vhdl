-- Seed: 13565971057323572524,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity wgcgufm is
  port (geuedw : in std_logic_vector(1 to 4));
end wgcgufm;

architecture wi of wgcgufm is
  
begin
  
end wi;

library ieee;
use ieee.std_logic_1164.all;

entity jckphatb is
  port (rziveqisam : in real; qkh : linkage std_logic; yk : inout std_logic_vector(1 downto 3));
end jckphatb;

library ieee;
use ieee.std_logic_1164.all;

architecture nqgv of jckphatb is
  signal nniwyn : std_logic_vector(1 to 4);
begin
  tgmwrpi : entity work.wgcgufm
    port map (geuedw => nniwyn);
  rz : entity work.wgcgufm
    port map (geuedw => nniwyn);
  zarpit : entity work.wgcgufm
    port map (geuedw => nniwyn);
  
  -- Multi-driven assignments
  nniwyn <= ('Z', '-', 'X', 'Z');
end nqgv;

library ieee;
use ieee.std_logic_1164.all;

entity nzqm is
  port (rco : out std_logic_vector(1 to 3); oefwmfbam : in integer);
end nzqm;

library ieee;
use ieee.std_logic_1164.all;

architecture xvnalds of nzqm is
  signal e : std_logic_vector(1 to 4);
  signal kbidlwr : std_logic_vector(1 downto 3);
  signal qkumnf : std_logic;
  signal bljeugdqo : real;
  signal mwrbaooe : std_logic_vector(1 to 4);
begin
  dfzzypkyi : entity work.wgcgufm
    port map (geuedw => mwrbaooe);
  bkpbdbl : entity work.jckphatb
    port map (rziveqisam => bljeugdqo, qkh => qkumnf, yk => kbidlwr);
  dhybzlviij : entity work.wgcgufm
    port map (geuedw => e);
  
  -- Single-driven assignments
  bljeugdqo <= 16#39D.237#;
  
  -- Multi-driven assignments
  e <= ('U', '-', '-', 'L');
  rco <= "ZH0";
  mwrbaooe <= ('Z', '0', 'Z', 'U');
  mwrbaooe <= ('H', 'X', 'L', '0');
end xvnalds;

entity racweir is
  port (cbpla : in real);
end racweir;

library ieee;
use ieee.std_logic_1164.all;

architecture u of racweir is
  signal q : std_logic_vector(1 to 4);
  signal mkvmt : std_logic_vector(1 downto 3);
  signal fchs : std_logic;
  signal zio : real;
begin
  tcbkfejvb : entity work.jckphatb
    port map (rziveqisam => zio, qkh => fchs, yk => mkvmt);
  grdjlozemm : entity work.wgcgufm
    port map (geuedw => q);
  
  -- Multi-driven assignments
  q <= "UH0-";
end u;



-- Seed after: 1719957702865004919,6882842853887419669
