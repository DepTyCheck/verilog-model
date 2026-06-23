-- Seed: 8757324284647004278,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity xmawrcf is
  port (ndwxfndlxk : inout std_logic; uftdhj : inout std_logic_vector(4 downto 0));
end xmawrcf;

architecture sdpieibq of xmawrcf is
  
begin
  -- Multi-driven assignments
  uftdhj <= "Z-WXZ";
  uftdhj <= ('Z', 'H', 'Z', 'U', '-');
  ndwxfndlxk <= '-';
  uftdhj <= "-HXH0";
end sdpieibq;

library ieee;
use ieee.std_logic_1164.all;

entity nc is
  port (psdkpexbyw : buffer bit; wv : inout std_logic_vector(0 to 1); kgzosex : out std_logic; rhi : linkage std_logic_vector(0 to 1));
end nc;

library ieee;
use ieee.std_logic_1164.all;

architecture njtfyivny of nc is
  signal c : std_logic_vector(4 downto 0);
  signal iyqvxc : std_logic_vector(4 downto 0);
begin
  dxlctnr : entity work.xmawrcf
    port map (ndwxfndlxk => kgzosex, uftdhj => iyqvxc);
  kcyp : entity work.xmawrcf
    port map (ndwxfndlxk => kgzosex, uftdhj => iyqvxc);
  npgavsh : entity work.xmawrcf
    port map (ndwxfndlxk => kgzosex, uftdhj => c);
  ggnlc : entity work.xmawrcf
    port map (ndwxfndlxk => kgzosex, uftdhj => iyqvxc);
  
  -- Single-driven assignments
  psdkpexbyw <= '0';
  
  -- Multi-driven assignments
  kgzosex <= 'H';
  kgzosex <= '0';
  c <= "U0X--";
  kgzosex <= '0';
end njtfyivny;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (fkgydnq : inout std_logic_vector(3 downto 1); zthifhm : inout real; hauytzrqlx : inout std_logic);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture pcskyh of w is
  signal skrnzwlpeg : std_logic;
  signal bzu : std_logic_vector(0 to 1);
  signal qghfnrgxgh : bit;
  signal nbezahl : std_logic_vector(0 to 1);
  signal tbd : std_logic_vector(0 to 1);
  signal hshazvtz : bit;
  signal fmnv : std_logic_vector(4 downto 0);
  signal jhivigib : std_logic;
begin
  txybwlw : entity work.xmawrcf
    port map (ndwxfndlxk => jhivigib, uftdhj => fmnv);
  nvsdwsl : entity work.xmawrcf
    port map (ndwxfndlxk => hauytzrqlx, uftdhj => fmnv);
  ugekmxi : entity work.nc
    port map (psdkpexbyw => hshazvtz, wv => tbd, kgzosex => hauytzrqlx, rhi => nbezahl);
  gn : entity work.nc
    port map (psdkpexbyw => qghfnrgxgh, wv => bzu, kgzosex => skrnzwlpeg, rhi => tbd);
  
  -- Single-driven assignments
  zthifhm <= 2_0_2_0_4.4;
  
  -- Multi-driven assignments
  hauytzrqlx <= '0';
  tbd <= "U1";
  jhivigib <= 'X';
  jhivigib <= 'L';
end pcskyh;



-- Seed after: 17432529816077300325,8421704836678237495
