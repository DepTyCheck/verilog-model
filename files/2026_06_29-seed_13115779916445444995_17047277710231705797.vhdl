-- Seed: 13115779916445444995,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity erclchwqk is
  port (kmkcif : buffer integer; rkdhev : inout std_logic_vector(3 to 4));
end erclchwqk;

architecture wyzflwh of erclchwqk is
  
begin
  -- Single-driven assignments
  kmkcif <= 8#5#;
  
  -- Multi-driven assignments
  rkdhev <= "0Z";
  rkdhev <= ('-', 'H');
  rkdhev <= ('-', '0');
  rkdhev <= ('Z', '-');
end wyzflwh;

library ieee;
use ieee.std_logic_1164.all;

entity wyisj is
  port (j : inout std_logic_vector(3 downto 1); sulolzjj : linkage std_logic);
end wyisj;

library ieee;
use ieee.std_logic_1164.all;

architecture rk of wyisj is
  signal ofikhkjwnx : std_logic_vector(3 to 4);
  signal qpuglxctks : integer;
  signal s : std_logic_vector(3 to 4);
  signal knllubrz : integer;
  signal wnja : std_logic_vector(3 to 4);
  signal robfmtsd : integer;
begin
  ibaewgr : entity work.erclchwqk
    port map (kmkcif => robfmtsd, rkdhev => wnja);
  ncvbcdsky : entity work.erclchwqk
    port map (kmkcif => knllubrz, rkdhev => s);
  fugrxzryxl : entity work.erclchwqk
    port map (kmkcif => qpuglxctks, rkdhev => ofikhkjwnx);
  
  -- Multi-driven assignments
  wnja <= ('H', '0');
end rk;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (xvjyl : linkage severity_level; z : inout boolean_vector(2 to 3); bs : inout std_logic_vector(3 to 3));
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture lyyfnryps of t is
  signal wwappr : std_logic_vector(3 to 4);
  signal xcybey : integer;
  signal plbgvr : std_logic;
  signal olmotsjhj : std_logic_vector(3 downto 1);
begin
  zmcwk : entity work.wyisj
    port map (j => olmotsjhj, sulolzjj => plbgvr);
  xgzaa : entity work.erclchwqk
    port map (kmkcif => xcybey, rkdhev => wwappr);
  
  -- Single-driven assignments
  z <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  plbgvr <= 'U';
  bs <= "0";
end lyyfnryps;

library ieee;
use ieee.std_logic_1164.all;

entity tvnjkdkl is
  port (fmuxsp : out integer; jkbkbx : in std_logic_vector(3 downto 3); mrhashhcj : out time; apopfokeb : linkage integer);
end tvnjkdkl;

library ieee;
use ieee.std_logic_1164.all;

architecture snzakqawl of tvnjkdkl is
  signal dtbszrwf : std_logic_vector(3 to 3);
  signal bmbxnq : boolean_vector(2 to 3);
  signal jvdbx : severity_level;
  signal rpavoyjar : integer;
  signal nvdphu : std_logic_vector(3 to 4);
begin
  wrcankc : entity work.erclchwqk
    port map (kmkcif => fmuxsp, rkdhev => nvdphu);
  jwktzrwvyz : entity work.erclchwqk
    port map (kmkcif => rpavoyjar, rkdhev => nvdphu);
  gwpd : entity work.t
    port map (xvjyl => jvdbx, z => bmbxnq, bs => dtbszrwf);
  
  -- Single-driven assignments
  mrhashhcj <= 16#4.AD# us;
end snzakqawl;



-- Seed after: 4047896931703441451,17047277710231705797
