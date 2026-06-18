-- Seed: 16718328395588729424,8118127366649987907

entity o is
  port (jbdd : linkage integer; wzkviyfh : in boolean; rqqplicbn : in boolean; qzghx : inout time_vector(0 to 0));
end o;

architecture jbjuijl of o is
  
begin
  -- Single-driven assignments
  qzghx <= (others => 1 hr);
end jbjuijl;

entity bwuxuvsd is
  port (vlvsf : out integer; tedzlppn : linkage integer_vector(2 to 3));
end bwuxuvsd;

architecture dwkn of bwuxuvsd is
  signal cm : time_vector(0 to 0);
  signal yopai : boolean;
  signal pk : boolean;
  signal qslsxmggtu : time_vector(0 to 0);
  signal vzfuvwf : boolean;
  signal wolzcxbq : integer;
  signal yzdlj : time_vector(0 to 0);
  signal zszyxbff : boolean;
  signal gusi : integer;
  signal siwjx : time_vector(0 to 0);
  signal jxfe : boolean;
  signal itcckch : integer;
begin
  ujxsuhohg : entity work.o
    port map (jbdd => itcckch, wzkviyfh => jxfe, rqqplicbn => jxfe, qzghx => siwjx);
  cabra : entity work.o
    port map (jbdd => gusi, wzkviyfh => jxfe, rqqplicbn => zszyxbff, qzghx => yzdlj);
  nkioc : entity work.o
    port map (jbdd => wolzcxbq, wzkviyfh => vzfuvwf, rqqplicbn => jxfe, qzghx => qslsxmggtu);
  dc : entity work.o
    port map (jbdd => vlvsf, wzkviyfh => pk, rqqplicbn => yopai, qzghx => cm);
  
  -- Single-driven assignments
  yopai <= FALSE;
  zszyxbff <= TRUE;
  jxfe <= FALSE;
  pk <= FALSE;
end dwkn;

entity x is
  port (wjui : out bit);
end x;

architecture j of x is
  signal gkd : time_vector(0 to 0);
  signal tuibzpgpm : boolean;
  signal vze : integer;
  signal eevzbl : time_vector(0 to 0);
  signal cxzfiiy : integer;
  signal ji : integer_vector(2 to 3);
  signal wudcmmyw : integer;
  signal manbhw : time_vector(0 to 0);
  signal uofameg : boolean;
  signal mifoak : boolean;
  signal bfkrvcgcy : integer;
begin
  fxh : entity work.o
    port map (jbdd => bfkrvcgcy, wzkviyfh => mifoak, rqqplicbn => uofameg, qzghx => manbhw);
  pdbrhdxlyv : entity work.bwuxuvsd
    port map (vlvsf => wudcmmyw, tedzlppn => ji);
  foyqzhpcr : entity work.o
    port map (jbdd => cxzfiiy, wzkviyfh => mifoak, rqqplicbn => uofameg, qzghx => eevzbl);
  qx : entity work.o
    port map (jbdd => vze, wzkviyfh => uofameg, rqqplicbn => tuibzpgpm, qzghx => gkd);
  
  -- Single-driven assignments
  wjui <= '1';
  uofameg <= TRUE;
  mifoak <= TRUE;
  tuibzpgpm <= TRUE;
end j;

library ieee;
use ieee.std_logic_1164.all;

entity sqmzzvt is
  port (laieakip : linkage string(2 to 4); a : in integer; rpsysg : buffer bit; owd : inout std_logic_vector(3 downto 2));
end sqmzzvt;

architecture eksbirbnzp of sqmzzvt is
  
begin
  hegdzlxnr : entity work.x
    port map (wjui => rpsysg);
  
  -- Multi-driven assignments
  owd <= "1U";
  owd <= ('H', 'H');
  owd <= "LX";
  owd <= "HH";
end eksbirbnzp;



-- Seed after: 4829688522344360366,8118127366649987907
