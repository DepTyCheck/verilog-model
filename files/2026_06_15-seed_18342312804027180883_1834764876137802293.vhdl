-- Seed: 18342312804027180883,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity by is
  port (gbm : in time; gh : in std_logic; ingbxh : inout time; nfaesgfbag : in integer);
end by;

architecture nfwnvyp of by is
  
begin
  -- Single-driven assignments
  ingbxh <= 0_2_2_0.0_0_1_3 us;
end nfwnvyp;

library ieee;
use ieee.std_logic_1164.all;

entity phwgbvtsl is
  port ( dzw : in std_logic_vector(1 downto 3)
  ; rh : linkage std_logic
  ; yonbqjibu : buffer integer_vector(2 to 1)
  ; nkw : inout std_logic_vector(4 downto 2)
  );
end phwgbvtsl;

architecture hmlgfvmvwq of phwgbvtsl is
  
begin
  -- Single-driven assignments
  yonbqjibu <= (others => 0);
  
  -- Multi-driven assignments
  nkw <= ('L', '-', 'Z');
  nkw <= ('L', '-', 'U');
end hmlgfvmvwq;

library ieee;
use ieee.std_logic_1164.all;

entity ptmkntdql is
  port (w : linkage bit_vector(2 to 1); nivughw : inout std_logic_vector(3 downto 3); didsz : linkage time);
end ptmkntdql;

library ieee;
use ieee.std_logic_1164.all;

architecture kkbhqta of ptmkntdql is
  signal tzxrlabci : integer;
  signal zg : time;
  signal paq : time;
  signal eziq : integer;
  signal dgkfywdtw : time;
  signal aue : time;
  signal xhk : std_logic_vector(4 downto 2);
  signal abcxxfgkit : integer_vector(2 to 1);
  signal qlqmu : std_logic;
  signal ydjdlsvsxr : std_logic_vector(1 downto 3);
begin
  psyirmhns : entity work.phwgbvtsl
    port map (dzw => ydjdlsvsxr, rh => qlqmu, yonbqjibu => abcxxfgkit, nkw => xhk);
  po : entity work.by
    port map (gbm => aue, gh => qlqmu, ingbxh => dgkfywdtw, nfaesgfbag => eziq);
  ias : entity work.by
    port map (gbm => paq, gh => qlqmu, ingbxh => zg, nfaesgfbag => tzxrlabci);
  
  -- Multi-driven assignments
  xhk <= "1W-";
  nivughw <= (others => '-');
  qlqmu <= 'W';
  nivughw <= "Z";
end kkbhqta;

entity k is
  port (zeppv : inout time);
end k;

architecture vqqa of k is
  
begin
  -- Single-driven assignments
  zeppv <= 8#64.5_1_1_0# ps;
end vqqa;



-- Seed after: 448106016147093573,1834764876137802293
