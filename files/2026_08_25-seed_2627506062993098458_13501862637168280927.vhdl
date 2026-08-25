-- Seed: 2627506062993098458,13501862637168280927

entity igujyehyl is
  port (zocqsd : in integer_vector(4 downto 3); hnzohsyzmv : in real; zimuvl : inout real_vector(2 downto 2));
end igujyehyl;

architecture nhtzlcdhs of igujyehyl is
  
begin
  
end nhtzlcdhs;

entity tazfbv is
  port (rhp : linkage time);
end tazfbv;

architecture hpmgda of tazfbv is
  signal ctkpwtpmmh : real_vector(2 downto 2);
  signal fnjemujr : integer_vector(4 downto 3);
  signal cf : real_vector(2 downto 2);
  signal sb : real;
  signal zedezgpi : real_vector(2 downto 2);
  signal clvy : real;
  signal atjmmlyfj : integer_vector(4 downto 3);
begin
  mzzuphf : entity work.igujyehyl
    port map (zocqsd => atjmmlyfj, hnzohsyzmv => clvy, zimuvl => zedezgpi);
  lynuaul : entity work.igujyehyl
    port map (zocqsd => atjmmlyfj, hnzohsyzmv => sb, zimuvl => cf);
  b : entity work.igujyehyl
    port map (zocqsd => fnjemujr, hnzohsyzmv => clvy, zimuvl => ctkpwtpmmh);
  
  -- Single-driven assignments
  clvy <= clvy;
  fnjemujr <= (16#AE#, 4324);
  sb <= clvy;
  atjmmlyfj <= (1_0_1_4_4, 8#0167#);
end hpmgda;

library ieee;
use ieee.std_logic_1164.all;

entity bwr is
  port (sgsbryx : out time; mqjww : inout std_logic_vector(1 to 0); vriesedqsw : buffer real; y : out std_logic);
end bwr;

architecture hmjt of bwr is
  
begin
  -- Single-driven assignments
  vriesedqsw <= 2_1_3.0;
  
  -- Multi-driven assignments
  y <= 'H';
  y <= y;
  y <= y;
end hmjt;



-- Seed after: 15826263938987279095,13501862637168280927
