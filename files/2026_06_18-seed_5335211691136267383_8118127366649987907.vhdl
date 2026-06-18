-- Seed: 5335211691136267383,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity rijah is
  port (qtudf : inout std_logic_vector(0 to 2); kxe : out std_logic);
end rijah;

architecture s of rijah is
  
begin
  -- Multi-driven assignments
  kxe <= 'L';
  kxe <= 'H';
end s;

library ieee;
use ieee.std_logic_1164.all;

entity xejshjtmxw is
  port (mhjjbsljo : in std_logic_vector(0 downto 0); a : inout real; ilfhvaocc : linkage integer; kdjwdw : inout std_logic);
end xejshjtmxw;

library ieee;
use ieee.std_logic_1164.all;

architecture towit of xejshjtmxw is
  signal iej : std_logic;
  signal f : std_logic_vector(0 to 2);
  signal lf : std_logic;
  signal piyve : std_logic_vector(0 to 2);
  signal pq : std_logic_vector(0 to 2);
begin
  zfgx : entity work.rijah
    port map (qtudf => pq, kxe => kdjwdw);
  xsspva : entity work.rijah
    port map (qtudf => piyve, kxe => lf);
  y : entity work.rijah
    port map (qtudf => f, kxe => iej);
  
  -- Multi-driven assignments
  f <= "L1L";
end towit;

library ieee;
use ieee.std_logic_1164.all;

entity ckyrpev is
  port (sksqg : buffer std_logic; sw : out real_vector(2 to 3); wmbyys : out boolean);
end ckyrpev;

library ieee;
use ieee.std_logic_1164.all;

architecture dxni of ckyrpev is
  signal dnszyb : integer;
  signal vmp : real;
  signal awrgwf : std_logic_vector(0 downto 0);
begin
  xgjzwmd : entity work.xejshjtmxw
    port map (mhjjbsljo => awrgwf, a => vmp, ilfhvaocc => dnszyb, kdjwdw => sksqg);
  
  -- Single-driven assignments
  wmbyys <= TRUE;
  sw <= (8#5_0.3_1_5_4_2#, 2#00.0_1_1_1_0#);
  
  -- Multi-driven assignments
  sksqg <= '1';
  sksqg <= 'W';
  sksqg <= 'L';
end dxni;



-- Seed after: 3664451892784541252,8118127366649987907
