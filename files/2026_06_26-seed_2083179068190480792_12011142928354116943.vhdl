-- Seed: 2083179068190480792,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity efeqixtnx is
  port (gvpdpfq : linkage std_logic_vector(3 to 1); vqzjfwlt : out severity_level);
end efeqixtnx;

architecture bc of efeqixtnx is
  
begin
  -- Single-driven assignments
  vqzjfwlt <= NOTE;
end bc;

entity gjatzbmobr is
  port (qxhyo : linkage time);
end gjatzbmobr;

library ieee;
use ieee.std_logic_1164.all;

architecture fqyvp of gjatzbmobr is
  signal lcwiyznkx : severity_level;
  signal bznwil : severity_level;
  signal guypvwi : std_logic_vector(3 to 1);
  signal xtxdbh : severity_level;
  signal ffnsfeiyrt : std_logic_vector(3 to 1);
  signal uoom : severity_level;
  signal pxyseaxqiq : std_logic_vector(3 to 1);
begin
  kuo : entity work.efeqixtnx
    port map (gvpdpfq => pxyseaxqiq, vqzjfwlt => uoom);
  hkl : entity work.efeqixtnx
    port map (gvpdpfq => ffnsfeiyrt, vqzjfwlt => xtxdbh);
  dkdhabv : entity work.efeqixtnx
    port map (gvpdpfq => guypvwi, vqzjfwlt => bznwil);
  v : entity work.efeqixtnx
    port map (gvpdpfq => pxyseaxqiq, vqzjfwlt => lcwiyznkx);
  
  -- Multi-driven assignments
  pxyseaxqiq <= "";
end fqyvp;

library ieee;
use ieee.std_logic_1164.all;

entity yblvwz is
  port (nh : out integer; rewld : in integer; dmh : out std_logic_vector(0 downto 4); goqvctwbz : out std_logic_vector(1 downto 1));
end yblvwz;

library ieee;
use ieee.std_logic_1164.all;

architecture uizmwtub of yblvwz is
  signal amviywb : severity_level;
  signal b : severity_level;
  signal kjbq : std_logic_vector(3 to 1);
  signal yajtdfss : severity_level;
  signal wiq : std_logic_vector(3 to 1);
begin
  loh : entity work.efeqixtnx
    port map (gvpdpfq => wiq, vqzjfwlt => yajtdfss);
  tuljimg : entity work.efeqixtnx
    port map (gvpdpfq => kjbq, vqzjfwlt => b);
  jdpegrq : entity work.efeqixtnx
    port map (gvpdpfq => dmh, vqzjfwlt => amviywb);
  
  -- Single-driven assignments
  nh <= 8#0_5#;
  
  -- Multi-driven assignments
  goqvctwbz <= (others => 'L');
  wiq <= (others => '0');
  goqvctwbz <= (others => '-');
  dmh <= (others => '0');
end uizmwtub;



-- Seed after: 8127901756036173104,12011142928354116943
