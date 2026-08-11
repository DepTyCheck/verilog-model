-- Seed: 15531321911981379444,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity yalfftzlrp is
  port (rcfh : inout time; ondy : buffer character; pakbmyawz : buffer std_logic_vector(1 downto 2));
end yalfftzlrp;

architecture sl of yalfftzlrp is
  
begin
  -- Multi-driven assignments
  pakbmyawz <= "";
end sl;

library ieee;
use ieee.std_logic_1164.all;

entity jibjt is
  port (gtz : in std_logic_vector(4 downto 2));
end jibjt;

library ieee;
use ieee.std_logic_1164.all;

architecture pvywmtep of jibjt is
  signal m : std_logic_vector(1 downto 2);
  signal edsivnued : character;
  signal fpab : time;
begin
  lmrhyhobh : entity work.yalfftzlrp
    port map (rcfh => fpab, ondy => edsivnued, pakbmyawz => m);
  
  -- Multi-driven assignments
  m <= m;
  m <= "";
end pvywmtep;

entity jlbhb is
  port (rbpneow : inout integer);
end jlbhb;

library ieee;
use ieee.std_logic_1164.all;

architecture vvxl of jlbhb is
  signal bhmjxqzecv : character;
  signal xxq : time;
  signal ncckwnnym : std_logic_vector(1 downto 2);
  signal yjrgt : character;
  signal hyvesy : time;
  signal vewblcrs : std_logic_vector(1 downto 2);
  signal phzxvowj : character;
  signal vcyoivs : time;
  signal jndnkoawtc : std_logic_vector(1 downto 2);
  signal f : character;
  signal otxhnuqq : time;
begin
  huagjky : entity work.yalfftzlrp
    port map (rcfh => otxhnuqq, ondy => f, pakbmyawz => jndnkoawtc);
  qduorlz : entity work.yalfftzlrp
    port map (rcfh => vcyoivs, ondy => phzxvowj, pakbmyawz => vewblcrs);
  nczhfs : entity work.yalfftzlrp
    port map (rcfh => hyvesy, ondy => yjrgt, pakbmyawz => ncckwnnym);
  rgv : entity work.yalfftzlrp
    port map (rcfh => xxq, ondy => bhmjxqzecv, pakbmyawz => jndnkoawtc);
  
  -- Single-driven assignments
  rbpneow <= 3_3_4_2_4;
  
  -- Multi-driven assignments
  vewblcrs <= (others => '0');
  vewblcrs <= jndnkoawtc;
  jndnkoawtc <= "";
  vewblcrs <= jndnkoawtc;
end vvxl;



-- Seed after: 322807529492472547,10594830431004325987
