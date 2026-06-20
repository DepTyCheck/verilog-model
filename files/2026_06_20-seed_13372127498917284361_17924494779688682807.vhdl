-- Seed: 13372127498917284361,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity iimstzl is
  port (uzrkmate : out std_logic_vector(0 downto 4); ufxdgxlbs : out std_logic_vector(2 to 0); mrwryyvqf : inout std_logic; zapj : buffer integer);
end iimstzl;

architecture nieudm of iimstzl is
  
begin
  -- Single-driven assignments
  zapj <= 4_0_2_0_1;
  
  -- Multi-driven assignments
  uzrkmate <= "";
end nieudm;

library ieee;
use ieee.std_logic_1164.all;

entity tvjkixr is
  port (wepipmjl : in boolean; gc : inout std_logic_vector(3 downto 2); rtieqlqclk : out std_logic; kwbmpyypo : in std_logic_vector(1 to 2));
end tvjkixr;

library ieee;
use ieee.std_logic_1164.all;

architecture gwfamrqvyl of tvjkixr is
  signal sackpi : integer;
  signal wtr : std_logic;
  signal p : integer;
  signal osrpnv : std_logic;
  signal vzqzllt : std_logic_vector(2 to 0);
  signal fangulx : std_logic_vector(0 downto 4);
  signal hxwtgbgs : integer;
  signal eivvfodam : std_logic_vector(2 to 0);
  signal q : std_logic_vector(0 downto 4);
  signal nuxgip : integer;
  signal tflwn : std_logic_vector(0 downto 4);
begin
  ldwfclbhr : entity work.iimstzl
    port map (uzrkmate => tflwn, ufxdgxlbs => tflwn, mrwryyvqf => rtieqlqclk, zapj => nuxgip);
  xuesgzp : entity work.iimstzl
    port map (uzrkmate => q, ufxdgxlbs => eivvfodam, mrwryyvqf => rtieqlqclk, zapj => hxwtgbgs);
  xtc : entity work.iimstzl
    port map (uzrkmate => fangulx, ufxdgxlbs => vzqzllt, mrwryyvqf => osrpnv, zapj => p);
  mydbvbjbss : entity work.iimstzl
    port map (uzrkmate => tflwn, ufxdgxlbs => eivvfodam, mrwryyvqf => wtr, zapj => sackpi);
  
  -- Multi-driven assignments
  vzqzllt <= "";
  tflwn <= (others => '0');
  wtr <= 'Z';
end gwfamrqvyl;



-- Seed after: 12788889420076573051,17924494779688682807
