-- Seed: 7200625064781851328,13903879141658024201



entity ww is
  port (fb : buffer bit_vector(2 downto 0); j : inout time; ydz : buffer real; ib : buffer real);
end ww;



architecture z of ww is
  
begin
  
end z;



entity vgnkwnqwwu is
  port (itnfprar : out severity_level);
end vgnkwnqwwu;



architecture wpxpyyb of vgnkwnqwwu is
  signal windzy : real;
  signal ejktvaceh : real;
  signal qnyf : time;
  signal erqgp : bit_vector(2 downto 0);
  signal opaz : real;
  signal dl : real;
  signal lhapikpsye : time;
  signal hwbi : bit_vector(2 downto 0);
  signal bx : real;
  signal vwgtxkrp : real;
  signal dkwctie : time;
  signal k : bit_vector(2 downto 0);
begin
  fspajgptl : entity work.ww
    port map (fb => k, j => dkwctie, ydz => vwgtxkrp, ib => bx);
  lrhot : entity work.ww
    port map (fb => hwbi, j => lhapikpsye, ydz => dl, ib => opaz);
  ofmxhp : entity work.ww
    port map (fb => erqgp, j => qnyf, ydz => ejktvaceh, ib => windzy);
end wpxpyyb;

library ieee;
use ieee.std_logic_1164.all;

entity zkxqsuv is
  port (w : out real; rjs : out std_logic_vector(4 downto 3); jwrbwl : buffer integer);
end zkxqsuv;



architecture zq of zkxqsuv is
  signal ihib : severity_level;
begin
  lzscgq : entity work.vgnkwnqwwu
    port map (itnfprar => ihib);
end zq;



entity fgjipvhsk is
  port (hhasstrzom : inout real; ok : inout bit_vector(3 downto 3));
end fgjipvhsk;

library ieee;
use ieee.std_logic_1164.all;

architecture cslsmo of fgjipvhsk is
  signal df : integer;
  signal qgjll : std_logic_vector(4 downto 3);
begin
  bygsciqzjn : entity work.zkxqsuv
    port map (w => hhasstrzom, rjs => qgjll, jwrbwl => df);
end cslsmo;



-- Seed after: 15225241189136263867,13903879141658024201
