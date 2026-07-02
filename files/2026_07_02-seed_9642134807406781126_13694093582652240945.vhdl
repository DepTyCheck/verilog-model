-- Seed: 9642134807406781126,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity zlrnbl is
  port (jtpiaqxfml : buffer std_logic_vector(0 downto 0));
end zlrnbl;

architecture bte of zlrnbl is
  
begin
  -- Multi-driven assignments
  jtpiaqxfml <= "0";
  jtpiaqxfml <= (others => 'H');
  jtpiaqxfml <= "L";
  jtpiaqxfml <= "Z";
end bte;

library ieee;
use ieee.std_logic_1164.all;

entity wmz is
  port (ebvqpa : out std_logic_vector(4 to 3); noiidr : inout severity_level);
end wmz;

library ieee;
use ieee.std_logic_1164.all;

architecture oztle of wmz is
  signal ykqpycqnwl : std_logic_vector(0 downto 0);
  signal zlnyab : std_logic_vector(0 downto 0);
  signal pgppfrns : std_logic_vector(0 downto 0);
begin
  xbsxu : entity work.zlrnbl
    port map (jtpiaqxfml => pgppfrns);
  yzlvk : entity work.zlrnbl
    port map (jtpiaqxfml => zlnyab);
  uxh : entity work.zlrnbl
    port map (jtpiaqxfml => ykqpycqnwl);
  
  -- Single-driven assignments
  noiidr <= NOTE;
  
  -- Multi-driven assignments
  pgppfrns <= (others => '-');
  ebvqpa <= "";
  ebvqpa <= "";
  zlnyab <= (others => '-');
end oztle;



-- Seed after: 10781105349318983482,13694093582652240945
