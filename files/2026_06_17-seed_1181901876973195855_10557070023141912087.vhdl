-- Seed: 1181901876973195855,10557070023141912087

entity cscwa is
  port (lkdyubjwi : inout character; nrqegdvbd : inout integer; hn : in time_vector(1 downto 2));
end cscwa;

architecture xnrdtjyn of cscwa is
  
begin
  -- Single-driven assignments
  lkdyubjwi <= 'm';
end xnrdtjyn;

entity ngovtyg is
  port (s : out time);
end ngovtyg;

architecture rhgzlc of ngovtyg is
  signal dxksmk : time_vector(1 downto 2);
  signal uqmhd : integer;
  signal gonq : character;
  signal olbgfupvr : time_vector(1 downto 2);
  signal hagqnohsm : integer;
  signal rt : character;
begin
  gukefiy : entity work.cscwa
    port map (lkdyubjwi => rt, nrqegdvbd => hagqnohsm, hn => olbgfupvr);
  lgbcbd : entity work.cscwa
    port map (lkdyubjwi => gonq, nrqegdvbd => uqmhd, hn => dxksmk);
  
  -- Single-driven assignments
  olbgfupvr <= (others => 0 ns);
  dxksmk <= (others => 0 ns);
  s <= 2_4_4_1.3 ns;
end rhgzlc;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (ghf : linkage std_logic);
end e;

architecture xaan of e is
  signal ekzwrpzrjh : integer;
  signal y : character;
  signal favebrumls : time_vector(1 downto 2);
  signal sqtsb : integer;
  signal rexnftqgba : character;
begin
  jauegocggm : entity work.cscwa
    port map (lkdyubjwi => rexnftqgba, nrqegdvbd => sqtsb, hn => favebrumls);
  byum : entity work.cscwa
    port map (lkdyubjwi => y, nrqegdvbd => ekzwrpzrjh, hn => favebrumls);
  
  -- Single-driven assignments
  favebrumls <= (others => 0 ns);
end xaan;



-- Seed after: 8616234318468440702,10557070023141912087
