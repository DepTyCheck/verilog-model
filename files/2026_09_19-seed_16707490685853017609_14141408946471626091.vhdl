-- Seed: 16707490685853017609,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (h : buffer std_logic_vector(4 downto 1); tcv : linkage bit_vector(0 to 1); q : inout time);
end e;

architecture bbhou of e is
  
begin
  -- Multi-driven assignments
  h <= "0--L";
  h <= h;
  h <= ('W', 'H', 'U', '1');
  h <= "XLWH";
end bbhou;

entity gsbrjaubzv is
  port (nycdpqjmvw : out time; vsaqgppvzb : in time);
end gsbrjaubzv;

architecture wt of gsbrjaubzv is
  
begin
  -- Single-driven assignments
  nycdpqjmvw <= 16#A# ms;
end wt;

entity ioms is
  port (yyxx : inout boolean; omtuxlitmk : in integer);
end ioms;

library ieee;
use ieee.std_logic_1164.all;

architecture yhuvzkmo of ioms is
  signal codjhiznw : time;
  signal ydjtgjo : bit_vector(0 to 1);
  signal vbnakk : time;
  signal wcldlqhr : bit_vector(0 to 1);
  signal nsvlg : std_logic_vector(4 downto 1);
begin
  jupuvtyg : entity work.e
    port map (h => nsvlg, tcv => wcldlqhr, q => vbnakk);
  ppqpavocdq : entity work.e
    port map (h => nsvlg, tcv => ydjtgjo, q => codjhiznw);
  
  -- Single-driven assignments
  yyxx <= TRUE;
end yhuvzkmo;



-- Seed after: 4691036531037490317,14141408946471626091
