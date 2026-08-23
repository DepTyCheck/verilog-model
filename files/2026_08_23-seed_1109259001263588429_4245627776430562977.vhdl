-- Seed: 1109259001263588429,4245627776430562977

entity dpbvvqhq is
  port (oywpt : inout real_vector(1 to 3); h : out time);
end dpbvvqhq;

architecture ksc of dpbvvqhq is
  
begin
  -- Single-driven assignments
  h <= 1 hr;
  oywpt <= (2#1_0_1_0.01#, 4_3_2_2_2.1, 2_2_0_2_0.2_2_2_2);
end ksc;

library ieee;
use ieee.std_logic_1164.all;

entity fntv is
  port (dvitlq : out boolean_vector(1 to 2); wqvyomyl : in std_logic_vector(4 to 0));
end fntv;

architecture nxlnhfkqv of fntv is
  signal yuyuaf : time;
  signal cjetgurfk : real_vector(1 to 3);
  signal opzocpclyl : time;
  signal ysptgdridz : real_vector(1 to 3);
begin
  gskdv : entity work.dpbvvqhq
    port map (oywpt => ysptgdridz, h => opzocpclyl);
  zr : entity work.dpbvvqhq
    port map (oywpt => cjetgurfk, h => yuyuaf);
  
  -- Single-driven assignments
  dvitlq <= (TRUE, FALSE);
end nxlnhfkqv;

entity gma is
  port (fileryadc : linkage integer_vector(1 to 2); mgsask : inout boolean);
end gma;

library ieee;
use ieee.std_logic_1164.all;

architecture utckuue of gma is
  signal khhivkktjd : time;
  signal ocdywqtn : real_vector(1 to 3);
  signal wvaegdil : std_logic_vector(4 to 0);
  signal zhpivh : boolean_vector(1 to 2);
begin
  ihgnopbgj : entity work.fntv
    port map (dvitlq => zhpivh, wqvyomyl => wvaegdil);
  rkgbea : entity work.dpbvvqhq
    port map (oywpt => ocdywqtn, h => khhivkktjd);
  
  -- Multi-driven assignments
  wvaegdil <= wvaegdil;
end utckuue;



-- Seed after: 4654541102057672485,4245627776430562977
