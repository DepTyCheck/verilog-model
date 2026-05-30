-- Seed: 4373385848829479885,1630680796402093529

library ieee;
use ieee.std_logic_1164.all;

entity vhalfsdvrs is
  port (oyrrxr : buffer time_vector(1 downto 3); t : linkage std_logic_vector(1 downto 4));
end vhalfsdvrs;



architecture mot of vhalfsdvrs is
  
begin
  
end mot;

library ieee;
use ieee.std_logic_1164.all;

entity oydny is
  port (rdjnozhl : linkage real; iwfyt : buffer real; uxbk : in time; acc : in std_logic);
end oydny;

library ieee;
use ieee.std_logic_1164.all;

architecture ez of oydny is
  signal vjfwvyv : time_vector(1 downto 3);
  signal djwy : std_logic_vector(1 downto 4);
  signal ivfyjodw : time_vector(1 downto 3);
  signal dxqebz : std_logic_vector(1 downto 4);
  signal bvyl : time_vector(1 downto 3);
begin
  vjceityyiu : entity work.vhalfsdvrs
    port map (oyrrxr => bvyl, t => dxqebz);
  j : entity work.vhalfsdvrs
    port map (oyrrxr => ivfyjodw, t => djwy);
  tlb : entity work.vhalfsdvrs
    port map (oyrrxr => vjfwvyv, t => dxqebz);
end ez;



-- Seed after: 1771820807775659179,1630680796402093529
