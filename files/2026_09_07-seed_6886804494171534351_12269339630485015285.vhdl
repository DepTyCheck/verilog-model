-- Seed: 6886804494171534351,12269339630485015285

entity vf is
  port (ranb : out severity_level);
end vf;

architecture vsv of vf is
  
begin
  -- Single-driven assignments
  ranb <= ranb;
end vsv;

library ieee;
use ieee.std_logic_1164.all;

entity psfb is
  port (std : in std_logic_vector(1 to 3); nx : buffer time_vector(4 downto 1); sp : buffer bit_vector(1 downto 1); dgfba : linkage time);
end psfb;

architecture wj of psfb is
  signal e : severity_level;
begin
  bacgrbk : entity work.vf
    port map (ranb => e);
  
  -- Single-driven assignments
  nx <= (33232.3 ns, 101 us, 8#6# fs, 8#4.2_0# us);
  sp <= (others => '0');
end wj;

entity xvgzffhhn is
  port (sirf : buffer time; nabgu : in integer; jyif : in character; qvzwe : out time);
end xvgzffhhn;

architecture fulevpzlx of xvgzffhhn is
  signal ksh : severity_level;
  signal amuffpazf : severity_level;
  signal h : severity_level;
begin
  pirzw : entity work.vf
    port map (ranb => h);
  vq : entity work.vf
    port map (ranb => amuffpazf);
  xnhfe : entity work.vf
    port map (ranb => ksh);
end fulevpzlx;



-- Seed after: 1081675856704202567,12269339630485015285
