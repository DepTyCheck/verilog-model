-- Seed: 13298729866994119701,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity gqqw is
  port (scxld : in character; ih : out time; echwcmzj : buffer std_logic_vector(4 to 1));
end gqqw;

architecture hnhyxt of gqqw is
  
begin
  -- Single-driven assignments
  ih <= 8#0_1_2_1# ms;
  
  -- Multi-driven assignments
  echwcmzj <= "";
  echwcmzj <= (others => '0');
  echwcmzj <= echwcmzj;
  echwcmzj <= "";
end hnhyxt;

library ieee;
use ieee.std_logic_1164.all;

entity szfahw is
  port (wgoaeoush : linkage real; giyl : buffer std_logic; xvxfymnlql : buffer character);
end szfahw;

architecture i of szfahw is
  
begin
  -- Single-driven assignments
  xvxfymnlql <= 's';
end i;

library ieee;
use ieee.std_logic_1164.all;

entity fbudxf is
  port ( tblqcz : linkage std_logic
  ; uwxrpgcq : linkage std_logic_vector(3 downto 2)
  ; cdhgyml : buffer time
  ; itflfsmiyq : linkage std_logic_vector(1 to 3)
  );
end fbudxf;

library ieee;
use ieee.std_logic_1164.all;

architecture yqpejhkp of fbudxf is
  signal bi : std_logic_vector(4 to 1);
  signal efzgjmut : std_logic_vector(4 to 1);
  signal yemcthas : time;
  signal diocvrzvo : character;
begin
  zt : entity work.gqqw
    port map (scxld => diocvrzvo, ih => yemcthas, echwcmzj => efzgjmut);
  dknncjd : entity work.gqqw
    port map (scxld => diocvrzvo, ih => cdhgyml, echwcmzj => bi);
  
  -- Single-driven assignments
  diocvrzvo <= 'g';
  
  -- Multi-driven assignments
  bi <= (others => '0');
  bi <= (others => '0');
end yqpejhkp;



-- Seed after: 2946677439725965416,3042374792655995433
