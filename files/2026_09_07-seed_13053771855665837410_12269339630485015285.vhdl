-- Seed: 13053771855665837410,12269339630485015285

entity pevw is
  port (jaalq : inout string(1 downto 1); gjxni : linkage time; nifvcvru : out integer; rsqpei : out time);
end pevw;

architecture lvx of pevw is
  
begin
  -- Single-driven assignments
  nifvcvru <= 2_4_1_3;
  jaalq <= (others => 'g');
  rsqpei <= 8#2.4_6_1# fs;
end lvx;

entity jdt is
  port (blurczky : inout real);
end jdt;

architecture taozcdyb of jdt is
  signal fjr : time;
  signal gnbefzcfo : integer;
  signal nwyhqfr : time;
  signal ipek : string(1 downto 1);
  signal ylybln : time;
  signal xfgooszkgx : integer;
  signal svbklsv : time;
  signal jahpp : string(1 downto 1);
  signal kg : time;
  signal z : integer;
  signal dfhxqwe : time;
  signal dgojkzb : string(1 downto 1);
begin
  rmfry : entity work.pevw
    port map (jaalq => dgojkzb, gjxni => dfhxqwe, nifvcvru => z, rsqpei => kg);
  gkiwyr : entity work.pevw
    port map (jaalq => jahpp, gjxni => svbklsv, nifvcvru => xfgooszkgx, rsqpei => ylybln);
  lzke : entity work.pevw
    port map (jaalq => ipek, gjxni => nwyhqfr, nifvcvru => gnbefzcfo, rsqpei => fjr);
  
  -- Single-driven assignments
  blurczky <= blurczky;
end taozcdyb;

library ieee;
use ieee.std_logic_1164.all;

entity gn is
  port (tsyypzh : buffer std_logic);
end gn;

architecture hwwcfmb of gn is
  signal xfqxxmp : real;
  signal vjer : time;
  signal g : integer;
  signal zobrn : time;
  signal xoloocaoi : string(1 downto 1);
  signal ybtpaoiyu : time;
  signal tt : integer;
  signal xckoox : time;
  signal vonrhcje : string(1 downto 1);
begin
  fv : entity work.pevw
    port map (jaalq => vonrhcje, gjxni => xckoox, nifvcvru => tt, rsqpei => ybtpaoiyu);
  tcwickc : entity work.pevw
    port map (jaalq => xoloocaoi, gjxni => zobrn, nifvcvru => g, rsqpei => vjer);
  tos : entity work.jdt
    port map (blurczky => xfqxxmp);
  
  -- Multi-driven assignments
  tsyypzh <= tsyypzh;
  tsyypzh <= '0';
  tsyypzh <= '1';
  tsyypzh <= tsyypzh;
end hwwcfmb;



-- Seed after: 14464071916321578778,12269339630485015285
