-- Seed: 14181437161999474445,6299883410057943775

entity ci is
  port (givqxrsevs : linkage severity_level; hbemwni : inout integer; uplpqxppq : buffer integer);
end ci;

architecture yr of ci is
  
begin
  -- Single-driven assignments
  uplpqxppq <= 16#2_B_3#;
  hbemwni <= uplpqxppq;
end yr;

entity ksdulqyvu is
  port (tsgmpzoo : out bit);
end ksdulqyvu;

architecture hce of ksdulqyvu is
  signal zwmuplh : integer;
  signal yytgsxzco : integer;
  signal tilpe : severity_level;
  signal pqtcnutt : integer;
  signal cghes : integer;
  signal xrvnmjiau : severity_level;
  signal nyttdxwvs : integer;
  signal prs : integer;
  signal n : severity_level;
  signal avvvvzue : integer;
  signal bjemowv : integer;
  signal qfxkykqh : severity_level;
begin
  pmsm : entity work.ci
    port map (givqxrsevs => qfxkykqh, hbemwni => bjemowv, uplpqxppq => avvvvzue);
  nygds : entity work.ci
    port map (givqxrsevs => n, hbemwni => prs, uplpqxppq => nyttdxwvs);
  xturrsnio : entity work.ci
    port map (givqxrsevs => xrvnmjiau, hbemwni => cghes, uplpqxppq => pqtcnutt);
  tcqaepbwwe : entity work.ci
    port map (givqxrsevs => tilpe, hbemwni => yytgsxzco, uplpqxppq => zwmuplh);
  
  -- Single-driven assignments
  tsgmpzoo <= '0';
end hce;

library ieee;
use ieee.std_logic_1164.all;

entity agc is
  port (gghfopwb : buffer std_logic; fmjvxiozhm : linkage string(3 downto 2); rai : inout integer; kww : inout std_logic_vector(3 downto 0));
end agc;

architecture vzwjtb of agc is
  signal po : bit;
  signal pmud : bit;
begin
  zvbyelzm : entity work.ksdulqyvu
    port map (tsgmpzoo => pmud);
  lppymymv : entity work.ksdulqyvu
    port map (tsgmpzoo => po);
  
  -- Multi-driven assignments
  gghfopwb <= '-';
  kww <= ('L', 'W', 'X', 'Z');
end vzwjtb;

library ieee;
use ieee.std_logic_1164.all;

entity gloghahat is
  port (xoovczhauj : linkage std_logic_vector(3 to 0); o : in integer);
end gloghahat;

library ieee;
use ieee.std_logic_1164.all;

architecture xgzbebiwtq of gloghahat is
  signal egidgr : bit;
  signal uyakttu : std_logic_vector(3 downto 0);
  signal iodpxa : integer;
  signal mjdi : string(3 downto 2);
  signal kmbrbju : std_logic;
  signal qwbvmm : integer;
  signal dxzo : integer;
  signal obdhnw : severity_level;
  signal vfkueeggwk : integer;
  signal me : integer;
  signal u : severity_level;
begin
  jpascswq : entity work.ci
    port map (givqxrsevs => u, hbemwni => me, uplpqxppq => vfkueeggwk);
  vxqwzok : entity work.ci
    port map (givqxrsevs => obdhnw, hbemwni => dxzo, uplpqxppq => qwbvmm);
  pr : entity work.agc
    port map (gghfopwb => kmbrbju, fmjvxiozhm => mjdi, rai => iodpxa, kww => uyakttu);
  v : entity work.ksdulqyvu
    port map (tsgmpzoo => egidgr);
  
  -- Multi-driven assignments
  uyakttu <= ('L', 'L', 'L', '1');
  kmbrbju <= kmbrbju;
end xgzbebiwtq;



-- Seed after: 4980621962207256914,6299883410057943775
