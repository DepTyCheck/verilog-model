-- Seed: 7757152515906813799,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity bbmxtecp is
  port (r : linkage time_vector(1 to 1); tfnrnqup : inout std_logic_vector(2 to 3); pp : linkage severity_level; bgcw : in time);
end bbmxtecp;

architecture ukmn of bbmxtecp is
  
begin
  -- Multi-driven assignments
  tfnrnqup <= tfnrnqup;
  tfnrnqup <= tfnrnqup;
end ukmn;

library ieee;
use ieee.std_logic_1164.all;

entity tgtd is
  port (vypbrlxnqv : in std_logic_vector(4 downto 1); pbwmtdshe : in severity_level; gzyf : buffer std_logic);
end tgtd;

architecture kzvwtbwsoo of tgtd is
  
begin
  
end kzvwtbwsoo;

entity yezgvs is
  port (gor : in time; anl : linkage time; emaxaxcnoh : in bit_vector(2 downto 4));
end yezgvs;

library ieee;
use ieee.std_logic_1164.all;

architecture wo of yezgvs is
  signal tmh : std_logic;
  signal qaqlftbfjl : severity_level;
  signal afqy : std_logic_vector(4 downto 1);
  signal evxhjrk : severity_level;
  signal gfdrwwo : std_logic_vector(2 to 3);
  signal eegt : time_vector(1 to 1);
  signal vpyqs : severity_level;
  signal smq : std_logic_vector(2 to 3);
  signal x : time_vector(1 to 1);
begin
  inxuxvv : entity work.bbmxtecp
    port map (r => x, tfnrnqup => smq, pp => vpyqs, bgcw => gor);
  isoej : entity work.bbmxtecp
    port map (r => eegt, tfnrnqup => gfdrwwo, pp => evxhjrk, bgcw => gor);
  gmp : entity work.tgtd
    port map (vypbrlxnqv => afqy, pbwmtdshe => qaqlftbfjl, gzyf => tmh);
  
  -- Single-driven assignments
  qaqlftbfjl <= ERROR;
  
  -- Multi-driven assignments
  afqy <= ('L', 'U', 'H', '-');
  gfdrwwo <= smq;
end wo;



-- Seed after: 1974249768829106889,5983430343285687595
