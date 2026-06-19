-- Seed: 17511298977844763728,3108530264173481209

entity sxigwvr is
  port (rqvizsi : buffer boolean_vector(0 to 3));
end sxigwvr;

architecture ljzxwnfsrh of sxigwvr is
  
begin
  -- Single-driven assignments
  rqvizsi <= (TRUE, FALSE, TRUE, TRUE);
end ljzxwnfsrh;

library ieee;
use ieee.std_logic_1164.all;

entity iio is
  port (lkszm : in integer_vector(4 downto 1); fzr : out real; ozrrtrd : out std_logic_vector(2 to 0); p : out bit);
end iio;

architecture lqji of iio is
  signal zupedfmbnt : boolean_vector(0 to 3);
  signal ew : boolean_vector(0 to 3);
  signal qsjvkiruv : boolean_vector(0 to 3);
begin
  mdkhsa : entity work.sxigwvr
    port map (rqvizsi => qsjvkiruv);
  piuakgeonl : entity work.sxigwvr
    port map (rqvizsi => ew);
  diqf : entity work.sxigwvr
    port map (rqvizsi => zupedfmbnt);
  
  -- Single-driven assignments
  p <= '0';
  fzr <= 0_2_2_2_4.0_3;
  
  -- Multi-driven assignments
  ozrrtrd <= (others => '0');
end lqji;



-- Seed after: 1996106081957807757,3108530264173481209
