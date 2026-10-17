-- Seed: 12532060942668775017,5444032936641997477

entity cxhnjdjpp is
  port (jdkdvbf : buffer real; l : out integer; lxuxxqq : out time_vector(0 to 4));
end cxhnjdjpp;

architecture admu of cxhnjdjpp is
  
begin
  -- Single-driven assignments
  lxuxxqq <= (0 hr, 2#010# ps, 21011 us, 2 sec, 0 hr);
end admu;

entity yhutxkitj is
  port (czenmy : in time);
end yhutxkitj;

architecture wagpf of yhutxkitj is
  signal h : time_vector(0 to 4);
  signal wjlwzvw : integer;
  signal qme : real;
begin
  zrxmsyzpi : entity work.cxhnjdjpp
    port map (jdkdvbf => qme, l => wjlwzvw, lxuxxqq => h);
end wagpf;

library ieee;
use ieee.std_logic_1164.all;

entity vnmm is
  port (cdpnklsi : buffer time; v : inout std_logic_vector(1 to 0));
end vnmm;

architecture psgmxy of vnmm is
  signal irdmxnx : time_vector(0 to 4);
  signal dcrhkc : integer;
  signal nppigbazty : real;
  signal gsypsdq : time;
begin
  w : entity work.yhutxkitj
    port map (czenmy => gsypsdq);
  izku : entity work.cxhnjdjpp
    port map (jdkdvbf => nppigbazty, l => dcrhkc, lxuxxqq => irdmxnx);
  
  -- Single-driven assignments
  cdpnklsi <= cdpnklsi;
  gsypsdq <= gsypsdq;
end psgmxy;

entity isyxvpicpx is
  port (rqyfydhwh : out time; ctjjefldv : linkage real; sxikdxthlh : buffer integer; jb : in integer);
end isyxvpicpx;

library ieee;
use ieee.std_logic_1164.all;

architecture stgo of isyxvpicpx is
  signal zjw : time_vector(0 to 4);
  signal kfmk : real;
  signal mnukybir : time_vector(0 to 4);
  signal nshdc : integer;
  signal eq : real;
  signal hcrilvnn : time_vector(0 to 4);
  signal la : integer;
  signal bj : real;
  signal uo : std_logic_vector(1 to 0);
  signal bog : time;
begin
  sdknzseh : entity work.vnmm
    port map (cdpnklsi => bog, v => uo);
  pfvanx : entity work.cxhnjdjpp
    port map (jdkdvbf => bj, l => la, lxuxxqq => hcrilvnn);
  oxaxjxm : entity work.cxhnjdjpp
    port map (jdkdvbf => eq, l => nshdc, lxuxxqq => mnukybir);
  svpvsjwksx : entity work.cxhnjdjpp
    port map (jdkdvbf => kfmk, l => sxikdxthlh, lxuxxqq => zjw);
  
  -- Single-driven assignments
  rqyfydhwh <= bog;
end stgo;



-- Seed after: 6313481576505386212,5444032936641997477
