-- Seed: 12270232080236734641,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity jydpw is
  port (rgq : out std_logic_vector(0 downto 3); l : in std_logic_vector(2 to 0));
end jydpw;

architecture avyfxdys of jydpw is
  
begin
  -- Multi-driven assignments
  rgq <= (others => '0');
end avyfxdys;

entity sk is
  port (azy : linkage integer);
end sk;

architecture gdwhjlsgyt of sk is
  
begin
  
end gdwhjlsgyt;

entity ixqqtfrl is
  port (bt : in integer);
end ixqqtfrl;

library ieee;
use ieee.std_logic_1164.all;

architecture itbtsmpsle of ixqqtfrl is
  signal qovdwhd : std_logic_vector(2 to 0);
  signal ennegbwpa : std_logic_vector(0 downto 3);
  signal lyehezce : std_logic_vector(0 downto 3);
  signal icfvflmhg : std_logic_vector(0 downto 3);
begin
  wtk : entity work.jydpw
    port map (rgq => icfvflmhg, l => lyehezce);
  uqnqtywh : entity work.jydpw
    port map (rgq => lyehezce, l => ennegbwpa);
  xectl : entity work.jydpw
    port map (rgq => ennegbwpa, l => qovdwhd);
  
  -- Multi-driven assignments
  icfvflmhg <= (others => '0');
  icfvflmhg <= (others => '0');
  icfvflmhg <= "";
  icfvflmhg <= (others => '0');
end itbtsmpsle;

entity i is
  port (nhhvnam : buffer time; bsjhato : linkage time; minxtfqtf : in bit; b : in integer);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture yiekqqbbnh of i is
  signal aslljmbmor : std_logic_vector(2 to 0);
  signal kikow : integer;
  signal alfoick : integer;
  signal iunsefz : std_logic_vector(2 to 0);
  signal mliiyw : std_logic_vector(0 downto 3);
begin
  tmnrgddmd : entity work.jydpw
    port map (rgq => mliiyw, l => iunsefz);
  msiwwbbuxm : entity work.sk
    port map (azy => alfoick);
  ggbske : entity work.sk
    port map (azy => kikow);
  mzc : entity work.jydpw
    port map (rgq => mliiyw, l => aslljmbmor);
end yiekqqbbnh;



-- Seed after: 14679907478638630049,10557070023141912087
