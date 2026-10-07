-- Seed: 18283726977758499834,5906004015519833893

entity wvkgllsakr is
  port (c : out bit; grklww : in time);
end wvkgllsakr;

architecture zribki of wvkgllsakr is
  
begin
  -- Single-driven assignments
  c <= '1';
end zribki;

entity kwowwnis is
  port (j : inout time; ug : linkage integer_vector(1 downto 0); evk : linkage real; gxelxrorbl : in integer);
end kwowwnis;

architecture alhucd of kwowwnis is
  signal h : time;
  signal mqgjkylhda : bit;
  signal wnnvowr : bit;
  signal cewdzqjfee : bit;
  signal fexzrep : time;
  signal oubc : bit;
begin
  xyho : entity work.wvkgllsakr
    port map (c => oubc, grklww => fexzrep);
  mnhtog : entity work.wvkgllsakr
    port map (c => cewdzqjfee, grklww => j);
  xbfjx : entity work.wvkgllsakr
    port map (c => wnnvowr, grklww => j);
  yx : entity work.wvkgllsakr
    port map (c => mqgjkylhda, grklww => h);
  
  -- Single-driven assignments
  j <= j;
  h <= j;
  fexzrep <= fexzrep;
end alhucd;

library ieee;
use ieee.std_logic_1164.all;

entity imnqp is
  port (h : buffer time; xwyav : buffer std_logic_vector(3 downto 1); gno : out boolean; mmq : buffer integer);
end imnqp;

architecture zgw of imnqp is
  signal v : time;
  signal xopafzqvin : bit;
  signal eizraauywq : time;
  signal j : bit;
  signal cgvjgtuy : integer;
  signal ttexfe : real;
  signal vkjgpx : integer_vector(1 downto 0);
  signal vlzgd : time;
  signal agocbhhh : bit;
begin
  vqjznzvrsk : entity work.wvkgllsakr
    port map (c => agocbhhh, grklww => h);
  w : entity work.kwowwnis
    port map (j => vlzgd, ug => vkjgpx, evk => ttexfe, gxelxrorbl => cgvjgtuy);
  zax : entity work.wvkgllsakr
    port map (c => j, grklww => eizraauywq);
  pfde : entity work.wvkgllsakr
    port map (c => xopafzqvin, grklww => v);
  
  -- Single-driven assignments
  cgvjgtuy <= mmq;
  h <= eizraauywq;
  mmq <= 01;
  eizraauywq <= 1 sec;
  
  -- Multi-driven assignments
  xwyav <= xwyav;
end zgw;



-- Seed after: 15448838154236509683,5906004015519833893
