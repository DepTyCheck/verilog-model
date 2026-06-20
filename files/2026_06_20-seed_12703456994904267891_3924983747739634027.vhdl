-- Seed: 12703456994904267891,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity hmi is
  port (lmkgyte : out std_logic; ublj : buffer integer; rh : inout string(5 to 1); eskjpkswc : in boolean_vector(2 downto 4));
end hmi;

architecture yatiqqr of hmi is
  
begin
  -- Single-driven assignments
  rh <= "";
  ublj <= 8#15#;
end yatiqqr;

library ieee;
use ieee.std_logic_1164.all;

entity slcenbu is
  port (pnmbssjfqs : inout real; m : buffer time; ucrqd : out std_logic_vector(3 downto 0); z : out real);
end slcenbu;

library ieee;
use ieee.std_logic_1164.all;

architecture ctmamxale of slcenbu is
  signal ojznbh : boolean_vector(2 downto 4);
  signal w : string(5 to 1);
  signal dyrk : integer;
  signal vufwofobt : std_logic;
  signal my : string(5 to 1);
  signal cynhecm : integer;
  signal gxuhs : string(5 to 1);
  signal iqpndra : integer;
  signal c : std_logic;
  signal jtaka : boolean_vector(2 downto 4);
  signal jlxnbgpsf : string(5 to 1);
  signal roolh : integer;
  signal ny : std_logic;
begin
  v : entity work.hmi
    port map (lmkgyte => ny, ublj => roolh, rh => jlxnbgpsf, eskjpkswc => jtaka);
  mbgkkj : entity work.hmi
    port map (lmkgyte => c, ublj => iqpndra, rh => gxuhs, eskjpkswc => jtaka);
  fbmbc : entity work.hmi
    port map (lmkgyte => ny, ublj => cynhecm, rh => my, eskjpkswc => jtaka);
  bxgmvmi : entity work.hmi
    port map (lmkgyte => vufwofobt, ublj => dyrk, rh => w, eskjpkswc => ojznbh);
  
  -- Single-driven assignments
  z <= 0.40;
  ojznbh <= (others => TRUE);
  m <= 1 hr;
  jtaka <= (others => TRUE);
  
  -- Multi-driven assignments
  ucrqd <= ('L', 'U', 'H', 'X');
end ctmamxale;

library ieee;
use ieee.std_logic_1164.all;

entity wkmwkhuhzz is
  port (sed : buffer std_logic; sysnn : buffer bit; ajg : buffer time; jh : inout real_vector(1 downto 0));
end wkmwkhuhzz;

architecture ux of wkmwkhuhzz is
  
begin
  -- Single-driven assignments
  ajg <= 16#92B# us;
  jh <= (16#6.D82B#, 1_4_1_3.1_4_0);
  sysnn <= '1';
end ux;

entity lnc is
  port (ux : in real; m : buffer severity_level; zsuigmj : linkage integer);
end lnc;

library ieee;
use ieee.std_logic_1164.all;

architecture yz of lnc is
  signal drl : string(5 to 1);
  signal juja : integer;
  signal cptdveu : std_logic;
  signal idccs : real_vector(1 downto 0);
  signal bw : time;
  signal wjswp : bit;
  signal wwqk : boolean_vector(2 downto 4);
  signal htxquyfd : string(5 to 1);
  signal hpdik : integer;
  signal hcebcxmntz : std_logic;
begin
  hwakkq : entity work.hmi
    port map (lmkgyte => hcebcxmntz, ublj => hpdik, rh => htxquyfd, eskjpkswc => wwqk);
  gpuhg : entity work.wkmwkhuhzz
    port map (sed => hcebcxmntz, sysnn => wjswp, ajg => bw, jh => idccs);
  faqdby : entity work.hmi
    port map (lmkgyte => cptdveu, ublj => juja, rh => drl, eskjpkswc => wwqk);
  
  -- Single-driven assignments
  wwqk <= (others => TRUE);
end yz;



-- Seed after: 17729848883933766171,3924983747739634027
