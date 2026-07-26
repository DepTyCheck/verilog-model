-- Seed: 7207742159796560839,7808623373429384027

entity mhpuj is
  port (dvbbm : linkage integer; e : in time);
end mhpuj;

architecture vxqjqoe of mhpuj is
  
begin
  
end vxqjqoe;

library ieee;
use ieee.std_logic_1164.all;

entity bqkt is
  port (lkavkzso : buffer bit_vector(4 downto 0); jfosxkwn : in std_logic; ezlohspquf : buffer time);
end bqkt;

architecture xmvljzahv of bqkt is
  signal mgz : time;
  signal xxocqugsn : integer;
  signal lt : time;
  signal xmxp : integer;
  signal jlgyn : integer;
  signal ucoubinxzn : integer;
begin
  dnknjxw : entity work.mhpuj
    port map (dvbbm => ucoubinxzn, e => ezlohspquf);
  xyu : entity work.mhpuj
    port map (dvbbm => jlgyn, e => ezlohspquf);
  dgujinse : entity work.mhpuj
    port map (dvbbm => xmxp, e => lt);
  laytctoh : entity work.mhpuj
    port map (dvbbm => xxocqugsn, e => mgz);
  
  -- Single-driven assignments
  mgz <= 34031 ns;
end xmvljzahv;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (iatl : inout std_logic; cispxtke : buffer character; ub : buffer integer);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture kccgec of c is
  signal azf : time;
  signal bisszhes : std_logic;
  signal b : bit_vector(4 downto 0);
  signal kcvjpe : time;
  signal itlvt : integer;
begin
  zaosogb : entity work.mhpuj
    port map (dvbbm => itlvt, e => kcvjpe);
  hsgpbpdu : entity work.bqkt
    port map (lkavkzso => b, jfosxkwn => bisszhes, ezlohspquf => azf);
  
  -- Single-driven assignments
  ub <= 16#F#;
  kcvjpe <= kcvjpe;
  cispxtke <= 'n';
  
  -- Multi-driven assignments
  iatl <= iatl;
  bisszhes <= iatl;
  iatl <= iatl;
end kccgec;



-- Seed after: 7058899654201993172,7808623373429384027
