-- Seed: 17202476518692721148,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity wan is
  port (eawfxl : inout std_logic);
end wan;

architecture nv of wan is
  
begin
  -- Multi-driven assignments
  eawfxl <= eawfxl;
  eawfxl <= 'Z';
  eawfxl <= eawfxl;
  eawfxl <= eawfxl;
end nv;

entity a is
  port (rhwb : in bit_vector(4 downto 3); v : inout integer; nhcl : in time; nftm : inout time);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture ncawhd of a is
  signal dla : std_logic;
begin
  niwlujge : entity work.wan
    port map (eawfxl => dla);
  hhdw : entity work.wan
    port map (eawfxl => dla);
  yrr : entity work.wan
    port map (eawfxl => dla);
  
  -- Multi-driven assignments
  dla <= dla;
  dla <= dla;
  dla <= dla;
  dla <= dla;
end ncawhd;

entity rfxeafq is
  port (stuzshn : out time);
end rfxeafq;

library ieee;
use ieee.std_logic_1164.all;

architecture uzl of rfxeafq is
  signal pgzmb : integer;
  signal fcefzrc : bit_vector(4 downto 3);
  signal e : std_logic;
  signal ghximwxrv : std_logic;
begin
  r : entity work.wan
    port map (eawfxl => ghximwxrv);
  vyjsickxn : entity work.wan
    port map (eawfxl => e);
  hsu : entity work.wan
    port map (eawfxl => e);
  cvafncxp : entity work.a
    port map (rhwb => fcefzrc, v => pgzmb, nhcl => stuzshn, nftm => stuzshn);
  
  -- Single-driven assignments
  fcefzrc <= fcefzrc;
  
  -- Multi-driven assignments
  ghximwxrv <= 'X';
  e <= ghximwxrv;
  ghximwxrv <= ghximwxrv;
  ghximwxrv <= 'U';
end uzl;



-- Seed after: 6975800887880368339,6000118208082478503
