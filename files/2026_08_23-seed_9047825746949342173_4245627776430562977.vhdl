-- Seed: 9047825746949342173,4245627776430562977

entity ytjdx is
  port (wld : buffer real_vector(0 to 0); rb : linkage real; cjiey : out time; kngeie : in boolean);
end ytjdx;

architecture eboehia of ytjdx is
  
begin
  -- Single-driven assignments
  cjiey <= cjiey;
  wld <= wld;
end eboehia;

entity igumjeloyu is
  port (ilgixhxrr : inout real; ptkpxjfpg : buffer real; ku : inout time);
end igumjeloyu;

architecture jdbtlcwwi of igumjeloyu is
  
begin
  -- Single-driven assignments
  ptkpxjfpg <= ptkpxjfpg;
  ku <= 8#0_3# ns;
  ilgixhxrr <= 16#D.EECC4#;
end jdbtlcwwi;

library ieee;
use ieee.std_logic_1164.all;

entity dnayujicxq is
  port (uw : inout std_logic_vector(1 downto 1); d : inout std_logic_vector(1 downto 3); qfsingmu : buffer std_logic_vector(4 to 0));
end dnayujicxq;

architecture qlzqymct of dnayujicxq is
  signal n : time;
  signal nnt : real;
  signal vfuuzxtkpl : real_vector(0 to 0);
  signal zvwetbs : boolean;
  signal odziljxgx : time;
  signal fwfpv : real;
  signal klqkoujf : real_vector(0 to 0);
  signal ytpuiqjvqb : time;
  signal o : real;
  signal qnzph : real;
begin
  rrpbwtii : entity work.igumjeloyu
    port map (ilgixhxrr => qnzph, ptkpxjfpg => o, ku => ytpuiqjvqb);
  otc : entity work.ytjdx
    port map (wld => klqkoujf, rb => fwfpv, cjiey => odziljxgx, kngeie => zvwetbs);
  vtpsnry : entity work.ytjdx
    port map (wld => vfuuzxtkpl, rb => nnt, cjiey => n, kngeie => zvwetbs);
  
  -- Multi-driven assignments
  uw <= (others => 'H');
  d <= (others => '0');
end qlzqymct;



-- Seed after: 7272634027311077889,4245627776430562977
