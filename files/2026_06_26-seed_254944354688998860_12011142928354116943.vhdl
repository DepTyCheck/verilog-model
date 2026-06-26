-- Seed: 254944354688998860,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity wdrflbi is
  port (vxzx : out string(1 to 2); fbh : in std_logic; zpviao : buffer boolean);
end wdrflbi;

architecture luk of wdrflbi is
  
begin
  -- Single-driven assignments
  zpviao <= TRUE;
  vxzx <= "on";
end luk;

entity towsfsyqu is
  port (epe : linkage bit_vector(3 to 2); fuvljy : buffer time; qvxjuh : buffer string(1 downto 3); anpnqmqh : linkage boolean_vector(1 downto 2));
end towsfsyqu;

library ieee;
use ieee.std_logic_1164.all;

architecture vxx of towsfsyqu is
  signal kdzdv : boolean;
  signal ez : std_logic;
  signal bniuwssfvs : string(1 to 2);
  signal hfkobvvk : boolean;
  signal oeys : string(1 to 2);
  signal qrq : boolean;
  signal fafkjalwdc : std_logic;
  signal vsh : string(1 to 2);
begin
  eozgnpkpy : entity work.wdrflbi
    port map (vxzx => vsh, fbh => fafkjalwdc, zpviao => qrq);
  uiydxdrs : entity work.wdrflbi
    port map (vxzx => oeys, fbh => fafkjalwdc, zpviao => hfkobvvk);
  pvcafdq : entity work.wdrflbi
    port map (vxzx => bniuwssfvs, fbh => ez, zpviao => kdzdv);
  
  -- Single-driven assignments
  qvxjuh <= (others => ' ');
  fuvljy <= 3221 ns;
  
  -- Multi-driven assignments
  ez <= 'U';
  fafkjalwdc <= '1';
  fafkjalwdc <= 'H';
  fafkjalwdc <= 'L';
end vxx;

library ieee;
use ieee.std_logic_1164.all;

entity caxowlhd is
  port (lcxpqnx : out std_logic; aeigohspo : out integer; tbnjh : inout std_logic_vector(4 downto 4));
end caxowlhd;

library ieee;
use ieee.std_logic_1164.all;

architecture skdh of caxowlhd is
  signal sbxuiisz : boolean;
  signal kvblruyw : std_logic;
  signal xoucxrl : string(1 to 2);
  signal nuzvrw : boolean;
  signal tnxfrjmdjw : std_logic;
  signal wtp : string(1 to 2);
begin
  ndgsseeusi : entity work.wdrflbi
    port map (vxzx => wtp, fbh => tnxfrjmdjw, zpviao => nuzvrw);
  ywam : entity work.wdrflbi
    port map (vxzx => xoucxrl, fbh => kvblruyw, zpviao => sbxuiisz);
  
  -- Multi-driven assignments
  lcxpqnx <= '1';
  tbnjh <= (others => 'W');
  tnxfrjmdjw <= 'H';
end skdh;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (f : in real; hjqrsuw : buffer std_logic);
end v;

architecture c of v is
  
begin
  -- Multi-driven assignments
  hjqrsuw <= '1';
  hjqrsuw <= 'X';
  hjqrsuw <= '1';
  hjqrsuw <= 'Z';
end c;



-- Seed after: 11577878963021540518,12011142928354116943
