-- Seed: 16956001172870241343,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity tha is
  port (nwxbjucn : out std_logic);
end tha;

architecture rehzrotsyo of tha is
  
begin
  -- Multi-driven assignments
  nwxbjucn <= 'U';
  nwxbjucn <= 'U';
  nwxbjucn <= 'H';
  nwxbjucn <= 'Z';
end rehzrotsyo;

library ieee;
use ieee.std_logic_1164.all;

entity ytozzvch is
  port (ktbzmgyi : in std_logic_vector(1 downto 1); rbkxnrc : out character; dkmm : inout string(3 downto 3));
end ytozzvch;

library ieee;
use ieee.std_logic_1164.all;

architecture qgunqy of ytozzvch is
  signal pfbhcrixft : std_logic;
begin
  izy : entity work.tha
    port map (nwxbjucn => pfbhcrixft);
  
  -- Single-driven assignments
  rbkxnrc <= 'h';
  dkmm <= (others => 'h');
  
  -- Multi-driven assignments
  pfbhcrixft <= '-';
  pfbhcrixft <= '-';
end qgunqy;

library ieee;
use ieee.std_logic_1164.all;

entity rmoglja is
  port (ostd : inout integer; jsxqlfqi : linkage time_vector(1 to 4); dzrpd : buffer std_logic_vector(2 downto 1); uw : buffer integer);
end rmoglja;

library ieee;
use ieee.std_logic_1164.all;

architecture htfgec of rmoglja is
  signal vvasg : std_logic;
  signal ykqy : std_logic;
begin
  vbwkfr : entity work.tha
    port map (nwxbjucn => ykqy);
  eoojtm : entity work.tha
    port map (nwxbjucn => vvasg);
  
  -- Single-driven assignments
  uw <= 2#0#;
  ostd <= 8#337#;
  
  -- Multi-driven assignments
  ykqy <= 'W';
  ykqy <= 'H';
end htfgec;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (yagbiafjja : buffer std_logic; rjoopsi : buffer time);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture vhrrmlo of b is
  signal hqqiyqv : string(3 downto 3);
  signal hprsmweihd : character;
  signal inf : std_logic_vector(1 downto 1);
begin
  ogljackprq : entity work.ytozzvch
    port map (ktbzmgyi => inf, rbkxnrc => hprsmweihd, dkmm => hqqiyqv);
  hagqr : entity work.tha
    port map (nwxbjucn => yagbiafjja);
  
  -- Single-driven assignments
  rjoopsi <= 2#1_0_0_1_0.1_0_0_0# ps;
  
  -- Multi-driven assignments
  yagbiafjja <= 'H';
  inf <= (others => 'L');
end vhrrmlo;



-- Seed after: 7017693198922457110,6882842853887419669
