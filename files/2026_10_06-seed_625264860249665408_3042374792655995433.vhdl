-- Seed: 625264860249665408,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity sjwpwo is
  port (hrbsgas : buffer std_logic; moubcbkmr : linkage integer_vector(0 downto 3); g : out integer);
end sjwpwo;

architecture cepsiiso of sjwpwo is
  
begin
  -- Single-driven assignments
  g <= 13;
  
  -- Multi-driven assignments
  hrbsgas <= hrbsgas;
  hrbsgas <= hrbsgas;
  hrbsgas <= hrbsgas;
end cepsiiso;

library ieee;
use ieee.std_logic_1164.all;

entity bfdrpeoghi is
  port (pvxk : in time; g : inout std_logic; t : linkage real);
end bfdrpeoghi;

library ieee;
use ieee.std_logic_1164.all;

architecture xujnu of bfdrpeoghi is
  signal ffbol : integer;
  signal oofphfg : integer_vector(0 downto 3);
  signal fvvqxzh : std_logic;
  signal ahnmfspx : integer;
  signal vaan : integer_vector(0 downto 3);
  signal ttsddnbkt : std_logic;
  signal hhlajaiqci : integer;
  signal r : integer_vector(0 downto 3);
  signal glofymb : std_logic;
begin
  ieh : entity work.sjwpwo
    port map (hrbsgas => glofymb, moubcbkmr => r, g => hhlajaiqci);
  lcwycv : entity work.sjwpwo
    port map (hrbsgas => ttsddnbkt, moubcbkmr => vaan, g => ahnmfspx);
  nqogtn : entity work.sjwpwo
    port map (hrbsgas => fvvqxzh, moubcbkmr => oofphfg, g => ffbol);
end xujnu;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (gpcuq : linkage bit_vector(4 downto 4); lorfgx : out std_logic; bogti : out time);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture tpqtifcol of f is
  signal flm : real;
  signal hn : std_logic;
  signal npgeavlbvs : time;
  signal sqjcgkdjir : real;
begin
  wn : entity work.bfdrpeoghi
    port map (pvxk => bogti, g => lorfgx, t => sqjcgkdjir);
  uzidm : entity work.bfdrpeoghi
    port map (pvxk => npgeavlbvs, g => hn, t => flm);
  
  -- Single-driven assignments
  bogti <= bogti;
  npgeavlbvs <= bogti;
end tpqtifcol;



-- Seed after: 7081975248456937348,3042374792655995433
