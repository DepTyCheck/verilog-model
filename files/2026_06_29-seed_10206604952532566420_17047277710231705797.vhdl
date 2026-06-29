-- Seed: 10206604952532566420,17047277710231705797

entity vzx is
  port (e : buffer bit_vector(3 downto 3); andklszcw : buffer integer);
end vzx;

architecture n of vzx is
  
begin
  -- Single-driven assignments
  e <= (others => '0');
end n;

entity jo is
  port (r : inout time; cxqzmrca : buffer boolean_vector(2 to 3));
end jo;

architecture lakevslhv of jo is
  signal kfzj : integer;
  signal dnbblcnaiy : bit_vector(3 downto 3);
begin
  xae : entity work.vzx
    port map (e => dnbblcnaiy, andklszcw => kfzj);
  
  -- Single-driven assignments
  cxqzmrca <= (FALSE, FALSE);
  r <= 1 hr;
end lakevslhv;

entity pksuhihcbi is
  port (a : in severity_level; ixbiucq : buffer time; sjckkoc : in real; gqtmllk : linkage time);
end pksuhihcbi;

architecture mgofu of pksuhihcbi is
  signal g : integer;
  signal czbdupmc : bit_vector(3 downto 3);
  signal ye : integer;
  signal yhvznu : bit_vector(3 downto 3);
  signal vwmvuu : integer;
  signal u : bit_vector(3 downto 3);
begin
  ru : entity work.vzx
    port map (e => u, andklszcw => vwmvuu);
  bevwekqxo : entity work.vzx
    port map (e => yhvznu, andklszcw => ye);
  q : entity work.vzx
    port map (e => czbdupmc, andklszcw => g);
end mgofu;

library ieee;
use ieee.std_logic_1164.all;

entity evg is
  port (smisfdmqm : inout bit_vector(1 to 3); uexviggmwc : out string(3 to 5); aiaxw : out std_logic);
end evg;

architecture fjbhh of evg is
  signal upz : time;
  signal afpzgrzeuq : real;
  signal jalldqkzqe : time;
  signal fa : severity_level;
  signal fludykof : integer;
  signal wdfrx : bit_vector(3 downto 3);
  signal ll : boolean_vector(2 to 3);
  signal yhuziwgl : time;
begin
  iwtxoy : entity work.jo
    port map (r => yhuziwgl, cxqzmrca => ll);
  hk : entity work.vzx
    port map (e => wdfrx, andklszcw => fludykof);
  g : entity work.pksuhihcbi
    port map (a => fa, ixbiucq => jalldqkzqe, sjckkoc => afpzgrzeuq, gqtmllk => upz);
  
  -- Single-driven assignments
  afpzgrzeuq <= 114.1;
  
  -- Multi-driven assignments
  aiaxw <= 'L';
  aiaxw <= 'H';
end fjbhh;



-- Seed after: 6707327066523068525,17047277710231705797
