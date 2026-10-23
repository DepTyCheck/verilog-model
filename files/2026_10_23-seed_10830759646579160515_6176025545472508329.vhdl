-- Seed: 10830759646579160515,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity euncy is
  port (gbbyrslvl : buffer std_logic_vector(3 downto 1); mqvrlkul : inout std_logic);
end euncy;

architecture iww of euncy is
  
begin
  -- Multi-driven assignments
  gbbyrslvl <= gbbyrslvl;
  mqvrlkul <= mqvrlkul;
  mqvrlkul <= '-';
end iww;

entity jvqcajl is
  port (mevbkmw : in bit; sthqgvrv : inout boolean_vector(4 downto 1); dimw : inout boolean_vector(0 downto 4));
end jvqcajl;

library ieee;
use ieee.std_logic_1164.all;

architecture uiv of jvqcajl is
  signal ka : std_logic;
  signal a : std_logic;
  signal d : std_logic_vector(3 downto 1);
begin
  ek : entity work.euncy
    port map (gbbyrslvl => d, mqvrlkul => a);
  wuykhk : entity work.euncy
    port map (gbbyrslvl => d, mqvrlkul => a);
  xwajscoikr : entity work.euncy
    port map (gbbyrslvl => d, mqvrlkul => ka);
  
  -- Multi-driven assignments
  ka <= 'L';
end uiv;



-- Seed after: 12850981015905603690,6176025545472508329
