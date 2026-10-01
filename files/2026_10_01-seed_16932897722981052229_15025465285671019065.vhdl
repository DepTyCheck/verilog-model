-- Seed: 16932897722981052229,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity pdawxxfe is
  port (nsucqbw : in real; pff : linkage std_logic_vector(0 downto 3); bo : linkage std_logic);
end pdawxxfe;

architecture a of pdawxxfe is
  
begin
  
end a;

entity qefb is
  port (ykw : linkage integer);
end qefb;

architecture yrgdvg of qefb is
  
begin
  
end yrgdvg;

library ieee;
use ieee.std_logic_1164.all;

entity wbthlvlnt is
  port (rucpcdik : linkage bit; l : buffer real; nayqdo : inout std_logic);
end wbthlvlnt;

library ieee;
use ieee.std_logic_1164.all;

architecture dtgrax of wbthlvlnt is
  signal oj : std_logic;
  signal upru : std_logic;
  signal rmjwwnfxca : std_logic_vector(0 downto 3);
  signal dgfmfvq : real;
begin
  nehtwxnscq : entity work.pdawxxfe
    port map (nsucqbw => dgfmfvq, pff => rmjwwnfxca, bo => upru);
  fnuhq : entity work.pdawxxfe
    port map (nsucqbw => l, pff => rmjwwnfxca, bo => oj);
  
  -- Single-driven assignments
  l <= l;
  dgfmfvq <= l;
  
  -- Multi-driven assignments
  rmjwwnfxca <= rmjwwnfxca;
  oj <= 'L';
end dtgrax;

entity ydv is
  port (tjrgnvmj : in time; ljj : linkage bit; xgshbuqbu : out real; fhmfdbzfi : out real_vector(2 to 1));
end ydv;

library ieee;
use ieee.std_logic_1164.all;

architecture d of ydv is
  signal jkts : real;
  signal jsmoz : std_logic;
  signal onx : std_logic_vector(0 downto 3);
  signal tbre : real;
  signal ifhawmkg : integer;
  signal jzufpsb : integer;
begin
  tvkgrmatue : entity work.qefb
    port map (ykw => jzufpsb);
  xprikp : entity work.qefb
    port map (ykw => ifhawmkg);
  ft : entity work.pdawxxfe
    port map (nsucqbw => tbre, pff => onx, bo => jsmoz);
  ylhjcfk : entity work.pdawxxfe
    port map (nsucqbw => jkts, pff => onx, bo => jsmoz);
  
  -- Single-driven assignments
  jkts <= 16#B.7_C_F_6_E#;
  fhmfdbzfi <= fhmfdbzfi;
  tbre <= 8#34640.2271#;
  xgshbuqbu <= xgshbuqbu;
  
  -- Multi-driven assignments
  onx <= (others => '0');
  jsmoz <= jsmoz;
  onx <= "";
end d;



-- Seed after: 10061243296388773280,15025465285671019065
