-- Seed: 4835051821199447740,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity fg is
  port (yq : out std_logic; ldkjhrwz : out character; kzkoyvhnqc : in boolean_vector(4 to 0));
end fg;

architecture pkszkuk of fg is
  
begin
  -- Single-driven assignments
  ldkjhrwz <= 'y';
  
  -- Multi-driven assignments
  yq <= 'U';
  yq <= yq;
end pkszkuk;

entity xm is
  port (fadb : inout integer; dnujdzs : in bit_vector(0 downto 4); r : linkage integer);
end xm;

library ieee;
use ieee.std_logic_1164.all;

architecture xken of xm is
  signal efomg : boolean_vector(4 to 0);
  signal u : character;
  signal xna : boolean_vector(4 to 0);
  signal limitd : character;
  signal lzsfsncn : boolean_vector(4 to 0);
  signal uiwzkbbo : character;
  signal cg : std_logic;
begin
  ddogbccxmi : entity work.fg
    port map (yq => cg, ldkjhrwz => uiwzkbbo, kzkoyvhnqc => lzsfsncn);
  psnakxplx : entity work.fg
    port map (yq => cg, ldkjhrwz => limitd, kzkoyvhnqc => xna);
  bixjnlsieo : entity work.fg
    port map (yq => cg, ldkjhrwz => u, kzkoyvhnqc => efomg);
  
  -- Single-driven assignments
  fadb <= 3;
  xna <= (others => TRUE);
  efomg <= efomg;
  lzsfsncn <= xna;
  
  -- Multi-driven assignments
  cg <= cg;
  cg <= 'H';
  cg <= cg;
  cg <= cg;
end xken;

entity dgxbk is
  port (cstqtgoj : in time; ry : linkage integer; zp : out string(1 to 2); erw : inout time);
end dgxbk;

architecture hjmazhpm of dgxbk is
  
begin
  -- Single-driven assignments
  erw <= 10 us;
  zp <= zp;
end hjmazhpm;

entity mqnempgvpo is
  port (mem : inout bit);
end mqnempgvpo;

library ieee;
use ieee.std_logic_1164.all;

architecture tmlx of mqnempgvpo is
  signal qwgsgkwkx : integer;
  signal ynudhn : bit_vector(0 downto 4);
  signal ibwrw : integer;
  signal yemssn : boolean_vector(4 to 0);
  signal eveghlebym : character;
  signal xoyrxx : std_logic;
begin
  rx : entity work.fg
    port map (yq => xoyrxx, ldkjhrwz => eveghlebym, kzkoyvhnqc => yemssn);
  ufwhhj : entity work.xm
    port map (fadb => ibwrw, dnujdzs => ynudhn, r => qwgsgkwkx);
  
  -- Single-driven assignments
  yemssn <= (others => TRUE);
  mem <= mem;
  
  -- Multi-driven assignments
  xoyrxx <= 'X';
  xoyrxx <= '-';
  xoyrxx <= 'U';
  xoyrxx <= 'H';
end tmlx;



-- Seed after: 11948136825422137306,3400751927341804175
