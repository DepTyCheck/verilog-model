-- Seed: 263214199767708737,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity iqp is
  port (dysmv : linkage integer_vector(4 to 4); zxa : in std_logic; fukearmzuu : inout std_logic_vector(4 downto 4));
end iqp;

architecture mtic of iqp is
  
begin
  -- Multi-driven assignments
  fukearmzuu <= (others => 'U');
  fukearmzuu <= (others => 'W');
end mtic;

library ieee;
use ieee.std_logic_1164.all;

entity qwh is
  port (vkzbw : in integer; vgemxnk : out real_vector(2 to 4); rks : buffer std_logic; vxnm : inout time);
end qwh;

architecture lsjucgfqso of qwh is
  
begin
  -- Single-driven assignments
  vxnm <= 0_4_0_4_2.331 ps;
  vgemxnk <= (8#3_1.7_1_5_0_1#, 8#1.7#, 1.0);
  
  -- Multi-driven assignments
  rks <= 'Z';
  rks <= 'L';
  rks <= '0';
  rks <= '-';
end lsjucgfqso;

library ieee;
use ieee.std_logic_1164.all;

entity clwycvly is
  port (wucrwruopz : out std_logic_vector(4 to 1));
end clwycvly;

library ieee;
use ieee.std_logic_1164.all;

architecture tw of clwycvly is
  signal bctazp : time;
  signal nsgayzh : real_vector(2 to 4);
  signal wxjp : integer;
  signal xkpeayygt : std_logic;
  signal esae : integer_vector(4 to 4);
  signal mq : std_logic_vector(4 downto 4);
  signal qjrg : std_logic;
  signal rwpqj : integer_vector(4 to 4);
begin
  n : entity work.iqp
    port map (dysmv => rwpqj, zxa => qjrg, fukearmzuu => mq);
  iuz : entity work.iqp
    port map (dysmv => esae, zxa => xkpeayygt, fukearmzuu => mq);
  ialznfs : entity work.qwh
    port map (vkzbw => wxjp, vgemxnk => nsgayzh, rks => qjrg, vxnm => bctazp);
  
  -- Multi-driven assignments
  qjrg <= '-';
  xkpeayygt <= '1';
  wucrwruopz <= "";
  wucrwruopz <= (others => '0');
end tw;

entity o is
  port (jahn : inout integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture ljgbxprcy of o is
  signal ygowvfg : std_logic_vector(4 downto 4);
  signal n : std_logic;
  signal gcwhwi : integer_vector(4 to 4);
begin
  vznj : entity work.iqp
    port map (dysmv => gcwhwi, zxa => n, fukearmzuu => ygowvfg);
  
  -- Single-driven assignments
  jahn <= 2#1#;
  
  -- Multi-driven assignments
  ygowvfg <= "X";
  n <= 'U';
end ljgbxprcy;



-- Seed after: 12939932819203484406,17047277710231705797
