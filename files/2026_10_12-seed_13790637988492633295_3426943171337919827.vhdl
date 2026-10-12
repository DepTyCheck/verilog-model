-- Seed: 13790637988492633295,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity byodhy is
  port (yakbeblx : inout time; jbavr : in std_logic_vector(2 downto 1));
end byodhy;

architecture cqntj of byodhy is
  
begin
  
end cqntj;

entity wmtak is
  port (yan : inout integer; bbhajzn : buffer real_vector(0 downto 2); mtogmyp : inout boolean; eoscano : buffer time);
end wmtak;

library ieee;
use ieee.std_logic_1164.all;

architecture nwraz of wmtak is
  signal mvnjwopav : std_logic_vector(2 downto 1);
  signal drhrzco : time;
  signal kvjdkmb : std_logic_vector(2 downto 1);
begin
  mzcboem : entity work.byodhy
    port map (yakbeblx => eoscano, jbavr => kvjdkmb);
  fzppuc : entity work.byodhy
    port map (yakbeblx => drhrzco, jbavr => mvnjwopav);
  
  -- Single-driven assignments
  mtogmyp <= mtogmyp;
  yan <= yan;
  bbhajzn <= bbhajzn;
end nwraz;

library ieee;
use ieee.std_logic_1164.all;

entity tgyprht is
  port (ayuwqhqa : out std_logic);
end tgyprht;

library ieee;
use ieee.std_logic_1164.all;

architecture y of tgyprht is
  signal wfuve : std_logic_vector(2 downto 1);
  signal uybzw : time;
  signal dlezvlju : time;
  signal ev : boolean;
  signal qxxdcnlcju : real_vector(0 downto 2);
  signal i : integer;
  signal qumsgq : time;
  signal h : std_logic_vector(2 downto 1);
  signal lfiqc : time;
begin
  ujzbplwicx : entity work.byodhy
    port map (yakbeblx => lfiqc, jbavr => h);
  wswbpvy : entity work.byodhy
    port map (yakbeblx => qumsgq, jbavr => h);
  pld : entity work.wmtak
    port map (yan => i, bbhajzn => qxxdcnlcju, mtogmyp => ev, eoscano => dlezvlju);
  ddfxvflu : entity work.byodhy
    port map (yakbeblx => uybzw, jbavr => wfuve);
end y;

library ieee;
use ieee.std_logic_1164.all;

entity yhxunmo is
  port (r : out std_logic_vector(3 downto 4); pytq : buffer integer; kc : buffer integer);
end yhxunmo;

library ieee;
use ieee.std_logic_1164.all;

architecture vbfgckku of yhxunmo is
  signal iebzcjztu : std_logic_vector(2 downto 1);
  signal zlla : time;
  signal egyn : std_logic;
begin
  hts : entity work.tgyprht
    port map (ayuwqhqa => egyn);
  lvljk : entity work.byodhy
    port map (yakbeblx => zlla, jbavr => iebzcjztu);
  
  -- Single-driven assignments
  kc <= 0_0_0_0;
  pytq <= 8#653#;
  
  -- Multi-driven assignments
  r <= "";
  r <= r;
  r <= r;
  egyn <= 'X';
end vbfgckku;



-- Seed after: 16222003098318798977,3426943171337919827
