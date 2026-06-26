-- Seed: 14809279903086517632,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity ms is
  port (mgqswcal : out integer; zgsqgst : inout std_logic; bu : linkage std_logic_vector(3 to 4));
end ms;

architecture hcm of ms is
  
begin
  -- Single-driven assignments
  mgqswcal <= 8#0_1_4_2#;
end hcm;

library ieee;
use ieee.std_logic_1164.all;

entity mjx is
  port (kguaigwb : inout integer; uutfa : in boolean; nssuhy : buffer std_logic);
end mjx;

library ieee;
use ieee.std_logic_1164.all;

architecture l of mjx is
  signal ptqahymwmr : std_logic_vector(3 to 4);
  signal bbs : std_logic;
  signal lkasbu : integer;
begin
  asphz : entity work.ms
    port map (mgqswcal => lkasbu, zgsqgst => bbs, bu => ptqahymwmr);
  
  -- Single-driven assignments
  kguaigwb <= 8#5041#;
end l;

entity kiqwwpnegi is
  port (jjawhameyz : linkage integer; jfw : buffer real; ovdxlh : buffer real; fupjtc : linkage real_vector(3 to 3));
end kiqwwpnegi;

library ieee;
use ieee.std_logic_1164.all;

architecture sus of kiqwwpnegi is
  signal rgm : std_logic_vector(3 to 4);
  signal u : integer;
  signal ktsgkk : std_logic;
  signal eef : boolean;
  signal qwca : integer;
begin
  ve : entity work.mjx
    port map (kguaigwb => qwca, uutfa => eef, nssuhy => ktsgkk);
  uawny : entity work.ms
    port map (mgqswcal => u, zgsqgst => ktsgkk, bu => rgm);
  
  -- Single-driven assignments
  eef <= TRUE;
  ovdxlh <= 0.0_3;
  jfw <= 16#3.5_4_9_0_D#;
  
  -- Multi-driven assignments
  ktsgkk <= '1';
  rgm <= "-Z";
  rgm <= "01";
end sus;



-- Seed after: 10089761737475594359,12011142928354116943
