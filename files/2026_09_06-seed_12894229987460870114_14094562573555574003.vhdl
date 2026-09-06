-- Seed: 12894229987460870114,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity zmf is
  port (lokqb : out std_logic_vector(0 downto 3); bhxfg : in std_logic_vector(1 downto 3));
end zmf;

architecture ohgyxoku of zmf is
  
begin
  -- Multi-driven assignments
  lokqb <= bhxfg;
  lokqb <= lokqb;
  lokqb <= "";
  lokqb <= lokqb;
end ohgyxoku;

entity kehg is
  port (fixwza : in bit_vector(4 to 0); vootzmx : in real);
end kehg;

library ieee;
use ieee.std_logic_1164.all;

architecture wqjtmsun of kehg is
  signal qeohmfwyqt : std_logic_vector(0 downto 3);
  signal xw : std_logic_vector(1 downto 3);
  signal h : std_logic_vector(1 downto 3);
begin
  xnqslhnsv : entity work.zmf
    port map (lokqb => h, bhxfg => xw);
  fic : entity work.zmf
    port map (lokqb => qeohmfwyqt, bhxfg => h);
  
  -- Multi-driven assignments
  xw <= "";
  xw <= "";
end wqjtmsun;



-- Seed after: 17194640264235706672,14094562573555574003
