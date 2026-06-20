-- Seed: 4507570908639907249,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity cluwwmwg is
  port (dveyhbjmjx : inout integer; jsibhlrmfk : inout boolean; fp : buffer std_logic_vector(0 downto 4); gsj : linkage real);
end cluwwmwg;

architecture exoalvttn of cluwwmwg is
  
begin
  -- Single-driven assignments
  jsibhlrmfk <= FALSE;
  dveyhbjmjx <= 16#E_8_1#;
end exoalvttn;

library ieee;
use ieee.std_logic_1164.all;

entity fm is
  port (zg : linkage bit; wbgl : buffer std_logic_vector(2 downto 4));
end fm;

library ieee;
use ieee.std_logic_1164.all;

architecture ptjc of fm is
  signal kvxscjdg : real;
  signal rglswbcucq : std_logic_vector(0 downto 4);
  signal pjiyii : boolean;
  signal u : integer;
begin
  perwh : entity work.cluwwmwg
    port map (dveyhbjmjx => u, jsibhlrmfk => pjiyii, fp => rglswbcucq, gsj => kvxscjdg);
  
  -- Multi-driven assignments
  rglswbcucq <= "";
  wbgl <= (others => '0');
  rglswbcucq <= (others => '0');
end ptjc;

library ieee;
use ieee.std_logic_1164.all;

entity soimfwa is
  port (kc : out std_logic; ide : inout std_logic_vector(0 to 2));
end soimfwa;

library ieee;
use ieee.std_logic_1164.all;

architecture vc of soimfwa is
  signal eygu : bit;
  signal thiln : bit;
  signal vxtrhvm : real;
  signal pokxyeiq : boolean;
  signal ls : integer;
  signal qawmbtst : real;
  signal nlvtbbcju : std_logic_vector(2 downto 4);
  signal btbgafkd : boolean;
  signal vydtfsfx : integer;
begin
  at : entity work.cluwwmwg
    port map (dveyhbjmjx => vydtfsfx, jsibhlrmfk => btbgafkd, fp => nlvtbbcju, gsj => qawmbtst);
  q : entity work.cluwwmwg
    port map (dveyhbjmjx => ls, jsibhlrmfk => pokxyeiq, fp => nlvtbbcju, gsj => vxtrhvm);
  t : entity work.fm
    port map (zg => thiln, wbgl => nlvtbbcju);
  ywgmcheup : entity work.fm
    port map (zg => eygu, wbgl => nlvtbbcju);
  
  -- Multi-driven assignments
  nlvtbbcju <= (others => '0');
  nlvtbbcju <= (others => '0');
  ide <= ('H', 'H', 'X');
end vc;

entity anrrlhgrbe is
  port (pr : inout boolean; eubmdjgs : out integer; gjk : in boolean; lmiklxtgzy : linkage boolean_vector(2 downto 0));
end anrrlhgrbe;

library ieee;
use ieee.std_logic_1164.all;

architecture vbuerftm of anrrlhgrbe is
  signal hdmy : std_logic;
  signal czm : std_logic_vector(0 to 2);
  signal jevplp : std_logic;
begin
  grbvlziyke : entity work.soimfwa
    port map (kc => jevplp, ide => czm);
  tifrlxib : entity work.soimfwa
    port map (kc => hdmy, ide => czm);
  
  -- Multi-driven assignments
  czm <= "LZL";
end vbuerftm;



-- Seed after: 7676799727142476705,3924983747739634027
