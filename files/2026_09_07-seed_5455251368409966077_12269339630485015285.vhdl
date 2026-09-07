-- Seed: 5455251368409966077,12269339630485015285

entity vfmffdqfam is
  port (k : linkage time; jrrmklggwm : in time);
end vfmffdqfam;

architecture dhtseq of vfmffdqfam is
  
begin
  
end dhtseq;

library ieee;
use ieee.std_logic_1164.all;

entity mzo is
  port (jhbez : in std_logic_vector(1 to 0); ubek : buffer integer);
end mzo;

architecture hx of mzo is
  signal spczzotv : time;
  signal vlhhamsct : time;
  signal cieysjnl : time;
  signal ugc : time;
  signal llropwerrt : time;
begin
  lsmvykd : entity work.vfmffdqfam
    port map (k => llropwerrt, jrrmklggwm => ugc);
  tubbpy : entity work.vfmffdqfam
    port map (k => cieysjnl, jrrmklggwm => vlhhamsct);
  aevytsf : entity work.vfmffdqfam
    port map (k => ugc, jrrmklggwm => vlhhamsct);
  fuufhdjt : entity work.vfmffdqfam
    port map (k => vlhhamsct, jrrmklggwm => spczzotv);
  
  -- Single-driven assignments
  ubek <= ubek;
  spczzotv <= cieysjnl;
end hx;

library ieee;
use ieee.std_logic_1164.all;

entity nt is
  port (f : buffer std_logic_vector(4 downto 2); ngsu : buffer time; gj : buffer time);
end nt;

architecture zdvwsco of nt is
  signal uwa : time;
begin
  y : entity work.vfmffdqfam
    port map (k => ngsu, jrrmklggwm => uwa);
  iow : entity work.vfmffdqfam
    port map (k => uwa, jrrmklggwm => gj);
  
  -- Single-driven assignments
  gj <= 4_4.02140 us;
  
  -- Multi-driven assignments
  f <= ('X', '-', '0');
  f <= "H00";
  f <= f;
end zdvwsco;



-- Seed after: 8951656416498046570,12269339630485015285
