-- Seed: 3150126265711806942,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity xwtpqztctb is
  port (ep : linkage integer; pggisuelgh : inout integer_vector(1 downto 2); yphxarc : out std_logic_vector(1 to 3));
end xwtpqztctb;

architecture dwde of xwtpqztctb is
  
begin
  -- Multi-driven assignments
  yphxarc <= ('X', '1', '1');
  yphxarc <= "LZX";
end dwde;

entity obf is
  port (q : in boolean);
end obf;

library ieee;
use ieee.std_logic_1164.all;

architecture btecbsckav of obf is
  signal kgmqto : integer_vector(1 downto 2);
  signal ocqgblnbca : integer;
  signal auzkqzu : integer_vector(1 downto 2);
  signal xgbx : integer;
  signal csfvjyzme : std_logic_vector(1 to 3);
  signal mxdt : integer_vector(1 downto 2);
  signal g : integer;
  signal vzuqnnf : std_logic_vector(1 to 3);
  signal upopt : integer_vector(1 downto 2);
  signal swrpo : integer;
begin
  nggkcjuzo : entity work.xwtpqztctb
    port map (ep => swrpo, pggisuelgh => upopt, yphxarc => vzuqnnf);
  aofyby : entity work.xwtpqztctb
    port map (ep => g, pggisuelgh => mxdt, yphxarc => csfvjyzme);
  lfmksgs : entity work.xwtpqztctb
    port map (ep => xgbx, pggisuelgh => auzkqzu, yphxarc => vzuqnnf);
  blfz : entity work.xwtpqztctb
    port map (ep => ocqgblnbca, pggisuelgh => kgmqto, yphxarc => vzuqnnf);
  
  -- Multi-driven assignments
  vzuqnnf <= ('Z', 'U', '0');
  vzuqnnf <= "Z0-";
  vzuqnnf <= csfvjyzme;
  vzuqnnf <= "Z0U";
end btecbsckav;

entity bmffoppf is
  port (z : inout integer; pknbqmnmc : in boolean; tvfuij : out boolean_vector(2 to 4); n : linkage string(1 downto 3));
end bmffoppf;

library ieee;
use ieee.std_logic_1164.all;

architecture fyxgnclay of bmffoppf is
  signal gsco : std_logic_vector(1 to 3);
  signal ftnr : integer_vector(1 downto 2);
  signal dlcwydd : integer;
begin
  ydcfbucjxk : entity work.xwtpqztctb
    port map (ep => dlcwydd, pggisuelgh => ftnr, yphxarc => gsco);
  yuhhp : entity work.obf
    port map (q => pknbqmnmc);
end fyxgnclay;



-- Seed after: 18076070318318032921,12536735141187299017
