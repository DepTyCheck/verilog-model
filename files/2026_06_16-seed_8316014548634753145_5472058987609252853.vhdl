-- Seed: 8316014548634753145,5472058987609252853

entity r is
  port (mz : out time_vector(3 to 3); odxuk : linkage integer);
end r;

architecture uszrywvy of r is
  
begin
  -- Single-driven assignments
  mz <= (others => 4.03 ns);
end uszrywvy;

entity ejedmn is
  port (j : buffer integer; rigstvdox : out integer; lkbskg : buffer integer; ppcxts : buffer integer);
end ejedmn;

architecture ybqgnn of ejedmn is
  signal csulig : time_vector(3 to 3);
  signal br : integer;
  signal i : time_vector(3 to 3);
  signal eqltxsd : time_vector(3 to 3);
  signal npviivyf : time_vector(3 to 3);
begin
  dtnkomr : entity work.r
    port map (mz => npviivyf, odxuk => ppcxts);
  zjoqydkz : entity work.r
    port map (mz => eqltxsd, odxuk => lkbskg);
  lyiawp : entity work.r
    port map (mz => i, odxuk => br);
  aiizo : entity work.r
    port map (mz => csulig, odxuk => j);
  
  -- Single-driven assignments
  rigstvdox <= 16#C#;
end ybqgnn;

library ieee;
use ieee.std_logic_1164.all;

entity krre is
  port (wks : in std_logic; egzybg : in character; eooajicryf : buffer time);
end krre;

architecture cb of krre is
  signal dmud : integer;
  signal x : time_vector(3 to 3);
  signal yxnltwydmp : integer;
  signal cw : time_vector(3 to 3);
begin
  viwwpqejg : entity work.r
    port map (mz => cw, odxuk => yxnltwydmp);
  ygcl : entity work.r
    port map (mz => x, odxuk => dmud);
  
  -- Single-driven assignments
  eooajicryf <= 2_3_2.322 ps;
end cb;

library ieee;
use ieee.std_logic_1164.all;

entity gqgmiam is
  port (ir : buffer std_logic_vector(4 downto 1));
end gqgmiam;

architecture dgubqsn of gqgmiam is
  signal d : integer;
  signal msvl : time_vector(3 to 3);
  signal nrdx : integer;
  signal fuldudjy : integer;
  signal cb : integer;
  signal bslvcx : integer;
begin
  bfukwtoitv : entity work.ejedmn
    port map (j => bslvcx, rigstvdox => cb, lkbskg => fuldudjy, ppcxts => nrdx);
  upnubqg : entity work.r
    port map (mz => msvl, odxuk => d);
  
  -- Multi-driven assignments
  ir <= "XLUU";
  ir <= ('0', '-', 'L', '-');
  ir <= ('X', '-', 'H', 'W');
end dgubqsn;



-- Seed after: 16022875018525783485,5472058987609252853
