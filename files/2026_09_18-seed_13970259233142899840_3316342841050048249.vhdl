-- Seed: 13970259233142899840,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity uvd is
  port (tzm : out std_logic; ykebbjp : in boolean_vector(2 to 1); bhwchkzrmv : in time);
end uvd;

architecture fihf of uvd is
  
begin
  
end fihf;

library ieee;
use ieee.std_logic_1164.all;

entity efhmieb is
  port (qgubce : in integer_vector(2 to 0); yxretj : inout real; nnnsvlpyh : out std_logic);
end efhmieb;

library ieee;
use ieee.std_logic_1164.all;

architecture rdputw of efhmieb is
  signal dawkmyrmlz : std_logic;
  signal dv : time;
  signal t : boolean_vector(2 to 1);
  signal bocmaxtd : std_logic;
  signal wwtyk : time;
  signal nxbkf : boolean_vector(2 to 1);
begin
  qecqqkhlrs : entity work.uvd
    port map (tzm => nnnsvlpyh, ykebbjp => nxbkf, bhwchkzrmv => wwtyk);
  rvrowqja : entity work.uvd
    port map (tzm => bocmaxtd, ykebbjp => t, bhwchkzrmv => dv);
  cplluzm : entity work.uvd
    port map (tzm => dawkmyrmlz, ykebbjp => t, bhwchkzrmv => wwtyk);
  
  -- Single-driven assignments
  yxretj <= 2#0.01001#;
  
  -- Multi-driven assignments
  dawkmyrmlz <= '0';
  nnnsvlpyh <= 'W';
  nnnsvlpyh <= nnnsvlpyh;
  nnnsvlpyh <= 'L';
end rdputw;

library ieee;
use ieee.std_logic_1164.all;

entity lvmz is
  port (k : in integer; oyd : inout time_vector(3 downto 1); lpmr : linkage std_logic; zpoxst : in integer);
end lvmz;

library ieee;
use ieee.std_logic_1164.all;

architecture pryjujl of lvmz is
  signal vgpju : time;
  signal wib : time;
  signal dnpwiwku : boolean_vector(2 to 1);
  signal hxyyao : std_logic;
begin
  zohexqcvjs : entity work.uvd
    port map (tzm => hxyyao, ykebbjp => dnpwiwku, bhwchkzrmv => wib);
  y : entity work.uvd
    port map (tzm => hxyyao, ykebbjp => dnpwiwku, bhwchkzrmv => vgpju);
  
  -- Single-driven assignments
  dnpwiwku <= (others => TRUE);
  vgpju <= 2#001.0# fs;
  wib <= wib;
  oyd <= oyd;
  
  -- Multi-driven assignments
  hxyyao <= 'W';
  hxyyao <= 'L';
  hxyyao <= '-';
  hxyyao <= 'Z';
end pryjujl;



-- Seed after: 7005715316915781231,3316342841050048249
