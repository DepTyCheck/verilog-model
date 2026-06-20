-- Seed: 2253920656277366098,3924983747739634027

entity idtehdii is
  port (cgef : in bit_vector(3 downto 1); tehj : out time; y : in time);
end idtehdii;

architecture tvs of idtehdii is
  
begin
  
end tvs;

entity zfankhi is
  port (vtbrjzlg : inout time; nrbou : linkage real; xi : buffer time);
end zfankhi;

architecture a of zfankhi is
  
begin
  
end a;

library ieee;
use ieee.std_logic_1164.all;

entity bu is
  port (ctnnxrcux : out time; gmvnupr : linkage std_logic_vector(0 downto 1));
end bu;

architecture vmhlyekuo of bu is
  signal dskzfda : time;
  signal gv : bit_vector(3 downto 1);
begin
  gsxy : entity work.idtehdii
    port map (cgef => gv, tehj => dskzfda, y => ctnnxrcux);
  
  -- Single-driven assignments
  gv <= ('0', '1', '0');
  ctnnxrcux <= 2#0.0# ps;
end vmhlyekuo;

entity gq is
  port (joxj : inout bit);
end gq;

architecture wksgjav of gq is
  signal ke : time;
  signal maf : real;
  signal wuhqgeu : time;
  signal hxhs : time;
  signal yzhtk : time;
  signal d : time;
  signal wvvpqz : bit_vector(3 downto 1);
begin
  ajhvnj : entity work.idtehdii
    port map (cgef => wvvpqz, tehj => d, y => yzhtk);
  s : entity work.idtehdii
    port map (cgef => wvvpqz, tehj => hxhs, y => wuhqgeu);
  mcqy : entity work.idtehdii
    port map (cgef => wvvpqz, tehj => wuhqgeu, y => d);
  axlrwvnts : entity work.zfankhi
    port map (vtbrjzlg => yzhtk, nrbou => maf, xi => ke);
  
  -- Single-driven assignments
  wvvpqz <= ('1', '0', '1');
end wksgjav;



-- Seed after: 3152571119928738271,3924983747739634027
