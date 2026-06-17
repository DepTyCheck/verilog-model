-- Seed: 12118152471214076269,10557070023141912087

entity coucotsrkt is
  port (v : buffer boolean);
end coucotsrkt;

architecture tjj of coucotsrkt is
  
begin
  -- Single-driven assignments
  v <= FALSE;
end tjj;

library ieee;
use ieee.std_logic_1164.all;

entity mfu is
  port (ayhpeyzj : buffer time; gezuxjwlsm : linkage std_logic_vector(0 downto 1));
end mfu;

architecture mefcjo of mfu is
  signal etgpfrjxl : boolean;
begin
  kodtj : entity work.coucotsrkt
    port map (v => etgpfrjxl);
  
  -- Single-driven assignments
  ayhpeyzj <= 16#B0# us;
end mefcjo;

library ieee;
use ieee.std_logic_1164.all;

entity ntjadhsa is
  port (ikvdam : out boolean; iierlpbg : in std_logic; ggmtd : inout integer);
end ntjadhsa;

library ieee;
use ieee.std_logic_1164.all;

architecture ztsb of ntjadhsa is
  signal vqzgsqhdz : boolean;
  signal w : std_logic_vector(0 downto 1);
  signal nzp : time;
begin
  frulxh : entity work.mfu
    port map (ayhpeyzj => nzp, gezuxjwlsm => w);
  outcnzayw : entity work.coucotsrkt
    port map (v => vqzgsqhdz);
  raqk : entity work.coucotsrkt
    port map (v => ikvdam);
  
  -- Single-driven assignments
  ggmtd <= 8#0#;
  
  -- Multi-driven assignments
  w <= (others => '0');
  w <= (others => '0');
  w <= (others => '0');
end ztsb;



-- Seed after: 2271989559667059087,10557070023141912087
