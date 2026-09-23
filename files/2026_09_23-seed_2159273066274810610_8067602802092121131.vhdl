-- Seed: 2159273066274810610,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (cbkdkijvd : inout std_logic_vector(0 downto 1); ahkbv : in real);
end v;

architecture aidxxbydy of v is
  
begin
  -- Multi-driven assignments
  cbkdkijvd <= (others => '0');
  cbkdkijvd <= (others => '0');
  cbkdkijvd <= cbkdkijvd;
end aidxxbydy;

library ieee;
use ieee.std_logic_1164.all;

entity phdd is
  port (y : in severity_level; s : out time; ynss : buffer std_logic_vector(0 to 1); hjveze : in integer);
end phdd;

library ieee;
use ieee.std_logic_1164.all;

architecture jxajy of phdd is
  signal eh : real;
  signal ztzsdnull : std_logic_vector(0 downto 1);
  signal zryy : real;
  signal kdoxvdmymm : std_logic_vector(0 downto 1);
  signal oxcds : real;
  signal u : std_logic_vector(0 downto 1);
begin
  qdkfdtxu : entity work.v
    port map (cbkdkijvd => u, ahkbv => oxcds);
  dmlcvw : entity work.v
    port map (cbkdkijvd => kdoxvdmymm, ahkbv => zryy);
  lnyqssvtw : entity work.v
    port map (cbkdkijvd => ztzsdnull, ahkbv => oxcds);
  sytirzswne : entity work.v
    port map (cbkdkijvd => ztzsdnull, ahkbv => eh);
  
  -- Single-driven assignments
  eh <= zryy;
  oxcds <= 1344.321;
  s <= 1 hr;
  zryy <= oxcds;
end jxajy;

library ieee;
use ieee.std_logic_1164.all;

entity ktoxioikon is
  port (gudgyoz : in integer; rx : inout real; mal : inout time; u : linkage std_logic);
end ktoxioikon;

library ieee;
use ieee.std_logic_1164.all;

architecture ovfunjllgy of ktoxioikon is
  signal gxfl : std_logic_vector(0 to 1);
  signal pmzluo : severity_level;
  signal yblwd : std_logic_vector(0 downto 1);
begin
  l : entity work.v
    port map (cbkdkijvd => yblwd, ahkbv => rx);
  uvuvthkwdp : entity work.phdd
    port map (y => pmzluo, s => mal, ynss => gxfl, hjveze => gudgyoz);
  
  -- Single-driven assignments
  rx <= 16#C_C.4#;
  pmzluo <= NOTE;
  
  -- Multi-driven assignments
  yblwd <= (others => '0');
  yblwd <= (others => '0');
end ovfunjllgy;

entity gbaoz is
  port (dx : buffer real_vector(0 downto 3); reixxyjq : inout time);
end gbaoz;

library ieee;
use ieee.std_logic_1164.all;

architecture ahugu of gbaoz is
  signal qdwwb : integer;
  signal uksrs : std_logic_vector(0 to 1);
  signal zple : severity_level;
  signal erfzgwdfkp : std_logic;
  signal us : time;
  signal zkwl : real;
  signal ia : integer;
begin
  njb : entity work.ktoxioikon
    port map (gudgyoz => ia, rx => zkwl, mal => us, u => erfzgwdfkp);
  xaldinutuc : entity work.phdd
    port map (y => zple, s => reixxyjq, ynss => uksrs, hjveze => qdwwb);
  
  -- Single-driven assignments
  zple <= zple;
  qdwwb <= 16#8#;
  ia <= 8#46337#;
  
  -- Multi-driven assignments
  erfzgwdfkp <= 'H';
end ahugu;



-- Seed after: 1162091386002460123,8067602802092121131
