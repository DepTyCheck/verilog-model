-- Seed: 15788098033177827574,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity yriri is
  port ( gzmeouqlyu : out std_logic
  ; qkjxybzfm : inout std_logic_vector(1 downto 4)
  ; iataza : in boolean_vector(1 to 2)
  ; gizfdkuvd : linkage std_logic_vector(4 downto 0)
  );
end yriri;

architecture kegojzyi of yriri is
  
begin
  -- Multi-driven assignments
  qkjxybzfm <= qkjxybzfm;
  qkjxybzfm <= "";
end kegojzyi;

library ieee;
use ieee.std_logic_1164.all;

entity looj is
  port (yp : linkage string(4 downto 5); ntvmjr : out std_logic);
end looj;

library ieee;
use ieee.std_logic_1164.all;

architecture q of looj is
  signal lqdnsqrfz : std_logic_vector(1 downto 4);
  signal eopiqf : std_logic;
  signal wyes : std_logic_vector(4 downto 0);
  signal o : std_logic;
  signal fdeyc : std_logic_vector(4 downto 0);
  signal aosoh : boolean_vector(1 to 2);
  signal cmcrkhcw : std_logic_vector(1 downto 4);
  signal mafgppwuu : std_logic;
begin
  tjcggg : entity work.yriri
    port map (gzmeouqlyu => mafgppwuu, qkjxybzfm => cmcrkhcw, iataza => aosoh, gizfdkuvd => fdeyc);
  jzocv : entity work.yriri
    port map (gzmeouqlyu => o, qkjxybzfm => cmcrkhcw, iataza => aosoh, gizfdkuvd => wyes);
  nlugpp : entity work.yriri
    port map (gzmeouqlyu => eopiqf, qkjxybzfm => lqdnsqrfz, iataza => aosoh, gizfdkuvd => wyes);
  ybomaxpj : entity work.yriri
    port map (gzmeouqlyu => o, qkjxybzfm => cmcrkhcw, iataza => aosoh, gizfdkuvd => fdeyc);
  
  -- Single-driven assignments
  aosoh <= aosoh;
  
  -- Multi-driven assignments
  ntvmjr <= ntvmjr;
  ntvmjr <= 'Z';
end q;



-- Seed after: 13286499947152473358,17234720251424330329
