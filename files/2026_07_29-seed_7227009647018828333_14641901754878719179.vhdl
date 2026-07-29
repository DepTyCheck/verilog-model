-- Seed: 7227009647018828333,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity kx is
  port (as : in std_logic_vector(0 downto 2); filqfkkps : out std_logic_vector(1 to 4); f : out integer);
end kx;

architecture ob of kx is
  
begin
  -- Multi-driven assignments
  filqfkkps <= "HL0U";
  filqfkkps <= filqfkkps;
  filqfkkps <= filqfkkps;
  filqfkkps <= "--ZH";
end ob;

entity w is
  port (iiifimez : inout severity_level; kttisfsdce : in integer; xcojj : inout integer; ggsjju : in time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture jpeahs of w is
  signal idgoxin : std_logic_vector(1 to 4);
  signal qojia : std_logic_vector(0 downto 2);
  signal eetcw : integer;
  signal cgh : std_logic_vector(0 downto 2);
  signal dmeflp : integer;
  signal zzb : std_logic_vector(1 to 4);
  signal lbcpwze : integer;
  signal ig : std_logic_vector(1 to 4);
  signal jijgjwl : std_logic_vector(0 downto 2);
begin
  lbqs : entity work.kx
    port map (as => jijgjwl, filqfkkps => ig, f => lbcpwze);
  smhdxdq : entity work.kx
    port map (as => jijgjwl, filqfkkps => zzb, f => dmeflp);
  pwoganmoj : entity work.kx
    port map (as => cgh, filqfkkps => zzb, f => eetcw);
  gn : entity work.kx
    port map (as => qojia, filqfkkps => idgoxin, f => xcojj);
  
  -- Single-driven assignments
  iiifimez <= FAILURE;
end jpeahs;



-- Seed after: 6147086371511439221,14641901754878719179
