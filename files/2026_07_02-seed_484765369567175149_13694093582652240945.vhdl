-- Seed: 484765369567175149,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity dtdvc is
  port (qech : inout std_logic_vector(4 downto 0); tztsuik : buffer std_logic);
end dtdvc;

architecture q of dtdvc is
  
begin
  -- Multi-driven assignments
  qech <= ('X', 'U', '1', '1', 'H');
  tztsuik <= 'U';
  tztsuik <= 'L';
  tztsuik <= 'L';
end q;

entity hs is
  port (fni : buffer real);
end hs;

library ieee;
use ieee.std_logic_1164.all;

architecture zhhpkgnb of hs is
  signal isilsxdme : std_logic_vector(4 downto 0);
  signal a : std_logic_vector(4 downto 0);
  signal h : std_logic;
  signal svthkr : std_logic_vector(4 downto 0);
begin
  yjgkqoj : entity work.dtdvc
    port map (qech => svthkr, tztsuik => h);
  qowwgomiqf : entity work.dtdvc
    port map (qech => a, tztsuik => h);
  kxphxtizdc : entity work.dtdvc
    port map (qech => isilsxdme, tztsuik => h);
  
  -- Single-driven assignments
  fni <= 0_1_2.1_3_2;
  
  -- Multi-driven assignments
  svthkr <= "1L0H0";
end zhhpkgnb;



-- Seed after: 16626745316878091284,13694093582652240945
