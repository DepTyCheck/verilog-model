-- Seed: 5815877928351658506,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity wiretz is
  port (irtiat : buffer std_logic; snbhtb : in integer; awpr : in std_logic; xns : linkage std_logic_vector(4 downto 3));
end wiretz;

architecture zq of wiretz is
  
begin
  
end zq;

library ieee;
use ieee.std_logic_1164.all;

entity jqltnumryr is
  port (nvqjwmoo : buffer std_logic_vector(0 to 3); dumxyqg : in boolean; xbqinu : linkage std_logic_vector(2 downto 3); fjrcu : in std_logic);
end jqltnumryr;

library ieee;
use ieee.std_logic_1164.all;

architecture yxlpm of jqltnumryr is
  signal enosra : integer;
  signal yyljqhdzpo : std_logic;
  signal wwmz : std_logic_vector(4 downto 3);
  signal ok : std_logic;
  signal cr : std_logic_vector(4 downto 3);
  signal kagkewx : std_logic;
  signal opeq : integer;
  signal nzxlrw : std_logic;
begin
  t : entity work.wiretz
    port map (irtiat => nzxlrw, snbhtb => opeq, awpr => kagkewx, xns => cr);
  ryxye : entity work.wiretz
    port map (irtiat => ok, snbhtb => opeq, awpr => ok, xns => wwmz);
  xphoe : entity work.wiretz
    port map (irtiat => yyljqhdzpo, snbhtb => enosra, awpr => yyljqhdzpo, xns => cr);
  
  -- Single-driven assignments
  opeq <= 2#0#;
  enosra <= 16#E_F_1_8_2#;
  
  -- Multi-driven assignments
  nvqjwmoo <= ('W', '0', 'W', '0');
  nvqjwmoo <= ('H', '0', '-', 'U');
end yxlpm;



-- Seed after: 4787961794100530458,6882842853887419669
