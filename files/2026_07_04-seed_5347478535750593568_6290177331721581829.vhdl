-- Seed: 5347478535750593568,6290177331721581829

use std.reflection.all;

entity te is
  port (okqpkz : inout value_mirror; efznjjvn : out integer_vector(2 downto 0));
end te;

architecture wasfcjza of te is
  
begin
  -- Single-driven assignments
  efznjjvn <= efznjjvn;
end wasfcjza;

library ieee;
use ieee.std_logic_1164.all;

entity wozznr is
  port (vsvycisrk : inout std_logic_vector(0 to 2); iomeijs : in integer);
end wozznr;

use std.reflection.all;

architecture qhsud of wozznr is
  signal cgdyde : integer_vector(2 downto 0);
  shared variable rdjyfjawx : value_mirror;
  signal nyebaqnpc : integer_vector(2 downto 0);
  shared variable tgdyvksnw : value_mirror;
  signal x : integer_vector(2 downto 0);
  shared variable tyd : value_mirror;
begin
  hzgjcsf : entity work.te
    port map (okqpkz => tyd, efznjjvn => x);
  szghm : entity work.te
    port map (okqpkz => tgdyvksnw, efznjjvn => nyebaqnpc);
  pptmrh : entity work.te
    port map (okqpkz => rdjyfjawx, efznjjvn => cgdyde);
  
  -- Multi-driven assignments
  vsvycisrk <= vsvycisrk;
end qhsud;



-- Seed after: 4321978922428784383,6290177331721581829
