-- Seed: 6576730634838568053,10871023049702252113

entity hfolczn is
  port (kvllstwcgw : buffer boolean_vector(3 downto 1); tg : out real_vector(4 downto 0));
end hfolczn;

architecture cakom of hfolczn is
  
begin
  -- Single-driven assignments
  tg <= (4232.0_4_0_3_1, 0430.3_1, 400.041, 8#6_5_0_2_5.23027#, 3_1.43203);
  kvllstwcgw <= (TRUE, FALSE, TRUE);
end cakom;

library ieee;
use ieee.std_logic_1164.all;

entity nzjowvdq is
  port (esojogengu : out real; ywuymaxy : buffer real; wtdmqdcemf : in time_vector(1 downto 0); jbmpduzv : buffer std_logic_vector(2 to 3));
end nzjowvdq;

architecture eqbozexbn of nzjowvdq is
  signal hmlnhqf : real_vector(4 downto 0);
  signal rwkuvmdw : boolean_vector(3 downto 1);
  signal msibv : real_vector(4 downto 0);
  signal gpaelvxwi : boolean_vector(3 downto 1);
begin
  go : entity work.hfolczn
    port map (kvllstwcgw => gpaelvxwi, tg => msibv);
  snxegw : entity work.hfolczn
    port map (kvllstwcgw => rwkuvmdw, tg => hmlnhqf);
  
  -- Multi-driven assignments
  jbmpduzv <= jbmpduzv;
  jbmpduzv <= "-Z";
  jbmpduzv <= jbmpduzv;
  jbmpduzv <= ('Z', '-');
end eqbozexbn;



-- Seed after: 17759945407735633512,10871023049702252113
