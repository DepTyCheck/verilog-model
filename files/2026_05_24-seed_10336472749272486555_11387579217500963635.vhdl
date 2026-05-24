-- Seed: 10336472749272486555,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity ghnk is
  port (oca : in std_logic_vector(3 downto 4));
end ghnk;



architecture c of ghnk is
  
begin
  
end c;

library ieee;
use ieee.std_logic_1164.all;

entity kds is
  port (qyxthtelg : inout std_logic);
end kds;

library ieee;
use ieee.std_logic_1164.all;

architecture gu of kds is
  signal pfblcuq : std_logic_vector(3 downto 4);
begin
  mgp : entity work.ghnk
    port map (oca => pfblcuq);
  fmtzkdgn : entity work.ghnk
    port map (oca => pfblcuq);
  tupaf : entity work.ghnk
    port map (oca => pfblcuq);
end gu;



-- Seed after: 14884381098342702602,11387579217500963635
