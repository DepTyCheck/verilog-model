-- Seed: 10407181968976245465,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (dgven : inout real_vector(0 to 4); yspauln : linkage real; m : out std_logic);
end n;

architecture qo of n is
  
begin
  -- Single-driven assignments
  dgven <= (2#1_1_0_0_1.0_1_0#, 22432.22310, 8#7.0_2#, 3.4_4_3_3_4, 1_3_4_1.2320);
  
  -- Multi-driven assignments
  m <= 'L';
  m <= 'U';
  m <= '-';
end qo;

library ieee;
use ieee.std_logic_1164.all;

entity hknkplhdf is
  port (o : inout integer; uocorin : buffer time; zwigh : buffer std_logic_vector(0 to 1));
end hknkplhdf;

library ieee;
use ieee.std_logic_1164.all;

architecture aku of hknkplhdf is
  signal uzzwjt : real;
  signal hyguepp : real_vector(0 to 4);
  signal poul : real;
  signal fevhli : real_vector(0 to 4);
  signal snfzpqolho : std_logic;
  signal snppb : real;
  signal jcjmcvwhqq : real_vector(0 to 4);
  signal bgcnoaqok : std_logic;
  signal fhccrlptqr : real;
  signal egwqdtbbj : real_vector(0 to 4);
begin
  cpzrpfsn : entity work.n
    port map (dgven => egwqdtbbj, yspauln => fhccrlptqr, m => bgcnoaqok);
  jsolhzqll : entity work.n
    port map (dgven => jcjmcvwhqq, yspauln => snppb, m => snfzpqolho);
  ib : entity work.n
    port map (dgven => fevhli, yspauln => poul, m => snfzpqolho);
  cpc : entity work.n
    port map (dgven => hyguepp, yspauln => uzzwjt, m => bgcnoaqok);
  
  -- Single-driven assignments
  uocorin <= 0.0_4_2 ms;
  
  -- Multi-driven assignments
  zwigh <= ('X', '0');
end aku;



-- Seed after: 15787080080148439031,14652815260262078753
