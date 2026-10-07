-- Seed: 15343710544478996930,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity bhq is
  port (tgtqkxoo : buffer std_logic_vector(1 to 1); xhel : in boolean; x : in std_logic_vector(4 to 3));
end bhq;

architecture vivfoglfy of bhq is
  
begin
  -- Multi-driven assignments
  tgtqkxoo <= (others => 'L');
  tgtqkxoo <= tgtqkxoo;
  tgtqkxoo <= (others => 'U');
  tgtqkxoo <= (others => 'H');
end vivfoglfy;

library ieee;
use ieee.std_logic_1164.all;

entity yl is
  port (xhsudjux : out time_vector(3 downto 1); alu : out std_logic);
end yl;

library ieee;
use ieee.std_logic_1164.all;

architecture rtmm of yl is
  signal schbitloy : std_logic_vector(4 to 3);
  signal myz : boolean;
  signal blbbaepz : std_logic_vector(4 to 3);
  signal olbwyatp : boolean;
  signal r : std_logic_vector(1 to 1);
begin
  evcolumo : entity work.bhq
    port map (tgtqkxoo => r, xhel => olbwyatp, x => blbbaepz);
  vh : entity work.bhq
    port map (tgtqkxoo => r, xhel => myz, x => schbitloy);
  
  -- Single-driven assignments
  xhsudjux <= (2#1_1_0_0_0# ms, 1 hr, 0_4_0_4_1.3_2 ms);
  myz <= TRUE;
  olbwyatp <= olbwyatp;
  
  -- Multi-driven assignments
  alu <= alu;
end rtmm;

entity xcjad is
  port (nihiftfoqu : out time);
end xcjad;

library ieee;
use ieee.std_logic_1164.all;

architecture svcazhuvir of xcjad is
  signal wkkjzxjf : std_logic_vector(4 to 3);
  signal nafaoq : boolean;
  signal hxkoeuteg : std_logic_vector(1 to 1);
  signal fncxnec : std_logic;
  signal twohfkfzd : time_vector(3 downto 1);
begin
  b : entity work.yl
    port map (xhsudjux => twohfkfzd, alu => fncxnec);
  oldsjvtsn : entity work.bhq
    port map (tgtqkxoo => hxkoeuteg, xhel => nafaoq, x => wkkjzxjf);
  
  -- Single-driven assignments
  nihiftfoqu <= 8#5_0.3_1# us;
  
  -- Multi-driven assignments
  fncxnec <= '0';
end svcazhuvir;

entity kv is
  port (aszzti : inout real);
end kv;

library ieee;
use ieee.std_logic_1164.all;

architecture wgzzkpoj of kv is
  signal lknhkc : std_logic_vector(4 to 3);
  signal exb : boolean;
  signal yehsmkti : std_logic_vector(1 to 1);
begin
  ydpf : entity work.bhq
    port map (tgtqkxoo => yehsmkti, xhel => exb, x => lknhkc);
end wgzzkpoj;



-- Seed after: 16064899874805220696,5906004015519833893
