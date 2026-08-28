-- Seed: 13696376575292848626,7198033922882419595

entity lksfphb is
  port (ugzqle : linkage real; n : buffer time_vector(0 downto 4); r : inout severity_level; awenwuql : linkage time);
end lksfphb;

architecture icymnxhowx of lksfphb is
  
begin
  -- Single-driven assignments
  r <= r;
  n <= (others => 0 ns);
end icymnxhowx;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (ddaq : buffer std_logic);
end c;

architecture xxao of c is
  signal rvibpxgamb : time;
  signal pvuejxubho : severity_level;
  signal davgrqa : time_vector(0 downto 4);
  signal rlqe : real;
  signal p : time;
  signal ryqqgg : severity_level;
  signal fehvx : time_vector(0 downto 4);
  signal wsvrvi : real;
begin
  ie : entity work.lksfphb
    port map (ugzqle => wsvrvi, n => fehvx, r => ryqqgg, awenwuql => p);
  xk : entity work.lksfphb
    port map (ugzqle => rlqe, n => davgrqa, r => pvuejxubho, awenwuql => rvibpxgamb);
  
  -- Multi-driven assignments
  ddaq <= 'H';
  ddaq <= '-';
  ddaq <= '0';
end xxao;

entity lqmr is
  port (lpjgmmcplo : in integer; bbftcmtiz : in boolean_vector(2 to 1); hnxfflbap : out real; b : buffer integer);
end lqmr;

library ieee;
use ieee.std_logic_1164.all;

architecture ho of lqmr is
  signal u : std_logic;
  signal ch : time;
  signal vqyicrg : severity_level;
  signal ltbi : time_vector(0 downto 4);
  signal lrvorprz : real;
  signal zjyeihi : std_logic;
begin
  nxiyycb : entity work.c
    port map (ddaq => zjyeihi);
  gudeacc : entity work.lksfphb
    port map (ugzqle => lrvorprz, n => ltbi, r => vqyicrg, awenwuql => ch);
  onlkf : entity work.c
    port map (ddaq => u);
  
  -- Single-driven assignments
  b <= 2#0_1_0_1_1#;
  hnxfflbap <= hnxfflbap;
  
  -- Multi-driven assignments
  zjyeihi <= 'H';
  u <= u;
  zjyeihi <= zjyeihi;
  zjyeihi <= zjyeihi;
end ho;



-- Seed after: 6168043911693077934,7198033922882419595
