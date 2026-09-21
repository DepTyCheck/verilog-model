-- Seed: 5695830532088181809,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity fjc is
  port (jwj : in std_logic_vector(3 to 3));
end fjc;

architecture bwqayblzq of fjc is
  
begin
  
end bwqayblzq;

entity xnmnhsf is
  port (uuinlzpch : buffer real; htrgkkwar : buffer time; szf : buffer real; aehjah : in real);
end xnmnhsf;

library ieee;
use ieee.std_logic_1164.all;

architecture cb of xnmnhsf is
  signal sphykpbxuo : std_logic_vector(3 to 3);
begin
  xcddsape : entity work.fjc
    port map (jwj => sphykpbxuo);
  
  -- Single-driven assignments
  uuinlzpch <= 2#0_1_1.1_0_0#;
  htrgkkwar <= 2#0_1# ps;
  szf <= 2#0_1_0_0_1.00110#;
  
  -- Multi-driven assignments
  sphykpbxuo <= sphykpbxuo;
  sphykpbxuo <= "L";
  sphykpbxuo <= sphykpbxuo;
  sphykpbxuo <= (others => 'H');
end cb;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (aje : out std_logic_vector(3 downto 3); jiumgeovvw : out boolean_vector(3 downto 0));
end a;

architecture shhkdny of a is
  
begin
  ehaiz : entity work.fjc
    port map (jwj => aje);
  
  -- Single-driven assignments
  jiumgeovvw <= jiumgeovvw;
  
  -- Multi-driven assignments
  aje <= "0";
end shhkdny;



-- Seed after: 13004010332674116103,12143220691580258643
