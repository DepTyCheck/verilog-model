-- Seed: 11859553773393885216,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity qaef is
  port (xn : linkage std_logic);
end qaef;

architecture rxbh of qaef is
  
begin
  
end rxbh;

library ieee;
use ieee.std_logic_1164.all;

entity hn is
  port (ppsvrg : out std_logic_vector(2 to 0); mssh : inout std_logic);
end hn;

architecture dqw of hn is
  
begin
  aidx : entity work.qaef
    port map (xn => mssh);
  wiguvgge : entity work.qaef
    port map (xn => mssh);
  qsaali : entity work.qaef
    port map (xn => mssh);
end dqw;

entity si is
  port (seri : buffer bit; yzjobu : buffer bit_vector(3 downto 0));
end si;

library ieee;
use ieee.std_logic_1164.all;

architecture nynfwjlgdn of si is
  signal tso : std_logic;
  signal szjwa : std_logic_vector(2 to 0);
begin
  gvcdmevu : entity work.hn
    port map (ppsvrg => szjwa, mssh => tso);
  goprqeidh : entity work.qaef
    port map (xn => tso);
  
  -- Single-driven assignments
  seri <= '0';
  yzjobu <= ('1', '1', '1', '0');
end nynfwjlgdn;



-- Seed after: 3347862519639514927,3924983747739634027
