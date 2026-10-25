-- Seed: 1552186975362450145,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity jyo is
  port (gthv : linkage std_logic_vector(4 to 1); qbbxll : in std_logic_vector(3 to 4));
end jyo;

architecture ceau of jyo is
  
begin
  
end ceau;

library ieee;
use ieee.std_logic_1164.all;

entity ldyla is
  port (vxinwedrm : in std_logic; f : out character; qreykbcnf : inout std_logic; qg : inout std_logic_vector(3 downto 1));
end ldyla;

library ieee;
use ieee.std_logic_1164.all;

architecture vhvrugouah of ldyla is
  signal nuxlfn : std_logic_vector(3 to 4);
  signal ueiistd : std_logic_vector(4 to 1);
  signal txchkte : std_logic_vector(3 to 4);
  signal ipvyndcvll : std_logic_vector(4 to 1);
begin
  bpmdpczmro : entity work.jyo
    port map (gthv => ipvyndcvll, qbbxll => txchkte);
  a : entity work.jyo
    port map (gthv => ueiistd, qbbxll => nuxlfn);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  qg <= qg;
  qreykbcnf <= '1';
end vhvrugouah;



-- Seed after: 12638569969205633494,13788294582928374589
