-- Seed: 11322758036858924752,3400751927341804175

entity tpzsaixb is
  port (yweblazrnc : out boolean_vector(4 to 4); guwinegcr : linkage time_vector(0 to 4); qcpdveht : in boolean_vector(3 downto 1));
end tpzsaixb;

architecture clpyudi of tpzsaixb is
  
begin
  -- Single-driven assignments
  yweblazrnc <= (others => TRUE);
end clpyudi;

entity dlvfi is
  port (dzpggokdj : linkage time; zvgmvzvsiw : out bit; usjpsdyaai : linkage severity_level);
end dlvfi;

architecture xcyjtjf of dlvfi is
  signal nlofk : boolean_vector(3 downto 1);
  signal psdvroo : time_vector(0 to 4);
  signal fonrax : boolean_vector(4 to 4);
begin
  feokqbtgp : entity work.tpzsaixb
    port map (yweblazrnc => fonrax, guwinegcr => psdvroo, qcpdveht => nlofk);
  
  -- Single-driven assignments
  nlofk <= nlofk;
  zvgmvzvsiw <= '0';
end xcyjtjf;

entity koidej is
  port (la : linkage string(3 downto 2));
end koidej;

architecture ed of koidej is
  signal obo : severity_level;
  signal sgh : bit;
  signal eeqzmmiiqy : time;
  signal p : time_vector(0 to 4);
  signal wew : boolean_vector(4 to 4);
  signal utqobxbiky : time_vector(0 to 4);
  signal eizqimldo : boolean_vector(4 to 4);
  signal f : boolean_vector(3 downto 1);
  signal ocjiowxq : time_vector(0 to 4);
  signal vncuh : boolean_vector(4 to 4);
begin
  zwtxswkr : entity work.tpzsaixb
    port map (yweblazrnc => vncuh, guwinegcr => ocjiowxq, qcpdveht => f);
  ssahy : entity work.tpzsaixb
    port map (yweblazrnc => eizqimldo, guwinegcr => utqobxbiky, qcpdveht => f);
  vzcio : entity work.tpzsaixb
    port map (yweblazrnc => wew, guwinegcr => p, qcpdveht => f);
  lkdentf : entity work.dlvfi
    port map (dzpggokdj => eeqzmmiiqy, zvgmvzvsiw => sgh, usjpsdyaai => obo);
  
  -- Single-driven assignments
  f <= f;
end ed;

library ieee;
use ieee.std_logic_1164.all;

entity ohvked is
  port (anh : in std_logic_vector(0 downto 4); hxqnxc : inout time; k : inout integer; brvxujpzc : out boolean);
end ohvked;

architecture hsluhe of ohvked is
  signal slyizbbz : severity_level;
  signal cwpkg : bit;
  signal azrjwp : time;
  signal vyknejkzm : string(3 downto 2);
  signal w : severity_level;
  signal bmajjskgag : bit;
  signal wydcstd : time;
  signal oiuyslt : string(3 downto 2);
begin
  hwpoxbshd : entity work.koidej
    port map (la => oiuyslt);
  vktthhqqnt : entity work.dlvfi
    port map (dzpggokdj => wydcstd, zvgmvzvsiw => bmajjskgag, usjpsdyaai => w);
  pkbqbzijji : entity work.koidej
    port map (la => vyknejkzm);
  u : entity work.dlvfi
    port map (dzpggokdj => azrjwp, zvgmvzvsiw => cwpkg, usjpsdyaai => slyizbbz);
  
  -- Single-driven assignments
  brvxujpzc <= FALSE;
  hxqnxc <= hxqnxc;
  k <= k;
end hsluhe;



-- Seed after: 9620855628430940513,3400751927341804175
