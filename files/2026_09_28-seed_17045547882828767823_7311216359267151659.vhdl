-- Seed: 17045547882828767823,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity owdp is
  port (ctysud : out std_logic; jxcdwenhxa : inout time);
end owdp;

architecture o of owdp is
  
begin
  -- Single-driven assignments
  jxcdwenhxa <= 4 sec;
  
  -- Multi-driven assignments
  ctysud <= ctysud;
end o;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (bewxwasv : in std_logic; r : linkage integer; dfvhqvr : in string(3 downto 4); icf : buffer integer);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture ku of a is
  signal hjwrh : time;
  signal kst : std_logic;
  signal nlvxckysj : time;
  signal xhvrrdtc : std_logic;
  signal jxb : time;
  signal kqwfa : std_logic;
  signal evxgfcsx : time;
  signal jri : std_logic;
begin
  vpoh : entity work.owdp
    port map (ctysud => jri, jxcdwenhxa => evxgfcsx);
  cka : entity work.owdp
    port map (ctysud => kqwfa, jxcdwenhxa => jxb);
  yympus : entity work.owdp
    port map (ctysud => xhvrrdtc, jxcdwenhxa => nlvxckysj);
  sdne : entity work.owdp
    port map (ctysud => kst, jxcdwenhxa => hjwrh);
  
  -- Single-driven assignments
  icf <= 8#1#;
  
  -- Multi-driven assignments
  jri <= kst;
  jri <= 'W';
end ku;

library ieee;
use ieee.std_logic_1164.all;

entity gctzucq is
  port (zq : in bit_vector(2 downto 0); olpxcilw : inout real; vlrce : inout std_logic; ub : inout real_vector(3 downto 3));
end gctzucq;

architecture njvb of gctzucq is
  signal gj : time;
  signal qomwctqdy : time;
  signal hafr : time;
begin
  hb : entity work.owdp
    port map (ctysud => vlrce, jxcdwenhxa => hafr);
  gjkhzgfx : entity work.owdp
    port map (ctysud => vlrce, jxcdwenhxa => qomwctqdy);
  flkbftd : entity work.owdp
    port map (ctysud => vlrce, jxcdwenhxa => gj);
  
  -- Single-driven assignments
  ub <= ub;
  olpxcilw <= 2.1_4_3_4;
end njvb;



-- Seed after: 10471759432481260054,7311216359267151659
