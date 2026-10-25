-- Seed: 1800863675995942401,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity usi is
  port (wlqfk : in std_logic);
end usi;

architecture mndhlx of usi is
  
begin
  
end mndhlx;

library ieee;
use ieee.std_logic_1164.all;

entity aage is
  port (xrvstopiw : inout std_logic);
end aage;

library ieee;
use ieee.std_logic_1164.all;

architecture ljiue of aage is
  signal gfoxjvn : std_logic;
  signal o : std_logic;
begin
  secbk : entity work.usi
    port map (wlqfk => xrvstopiw);
  rq : entity work.usi
    port map (wlqfk => o);
  g : entity work.usi
    port map (wlqfk => gfoxjvn);
  
  -- Multi-driven assignments
  gfoxjvn <= xrvstopiw;
end ljiue;

entity t is
  port (nmsyybr : inout integer; gxwrm : buffer integer);
end t;

architecture rvok of t is
  
begin
  -- Single-driven assignments
  gxwrm <= 2#0_0_0#;
  nmsyybr <= 8#4#;
end rvok;



-- Seed after: 14865172031577102308,13788294582928374589
