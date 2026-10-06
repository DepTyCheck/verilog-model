-- Seed: 8190542807803831457,3042374792655995433

entity s is
  port (cqdqkni : linkage integer; agxv : in severity_level; vztdh : in time_vector(3 to 4));
end s;

architecture xqwnrhr of s is
  
begin
  
end xqwnrhr;

library ieee;
use ieee.std_logic_1164.all;

entity ajezwdr is
  port (lygbxtvdel : linkage time; vuxdwotz : buffer std_logic_vector(4 downto 1));
end ajezwdr;

architecture gbfjjll of ajezwdr is
  signal vkfwsel : integer;
  signal xgjzcvpo : integer;
  signal meriazkok : time_vector(3 to 4);
  signal wfmmije : severity_level;
  signal kaemdlc : integer;
begin
  leiyhbcc : entity work.s
    port map (cqdqkni => kaemdlc, agxv => wfmmije, vztdh => meriazkok);
  jbgvridnge : entity work.s
    port map (cqdqkni => xgjzcvpo, agxv => wfmmije, vztdh => meriazkok);
  ltfbqpli : entity work.s
    port map (cqdqkni => vkfwsel, agxv => wfmmije, vztdh => meriazkok);
  
  -- Multi-driven assignments
  vuxdwotz <= vuxdwotz;
end gbfjjll;

entity kai is
  port (wjdx : buffer integer);
end kai;

library ieee;
use ieee.std_logic_1164.all;

architecture vkrmqa of kai is
  signal ayi : std_logic_vector(4 downto 1);
  signal df : time;
  signal rvsgtvfv : std_logic_vector(4 downto 1);
  signal xbcorm : time;
  signal hn : severity_level;
  signal zlr : time_vector(3 to 4);
  signal fvrdeh : severity_level;
  signal jfldqemtyk : integer;
begin
  hqnu : entity work.s
    port map (cqdqkni => jfldqemtyk, agxv => fvrdeh, vztdh => zlr);
  l : entity work.s
    port map (cqdqkni => wjdx, agxv => hn, vztdh => zlr);
  bwcglfxny : entity work.ajezwdr
    port map (lygbxtvdel => xbcorm, vuxdwotz => rvsgtvfv);
  yrmciulio : entity work.ajezwdr
    port map (lygbxtvdel => df, vuxdwotz => ayi);
  
  -- Single-driven assignments
  fvrdeh <= NOTE;
  hn <= WARNING;
  zlr <= zlr;
  
  -- Multi-driven assignments
  rvsgtvfv <= rvsgtvfv;
end vkrmqa;



-- Seed after: 6562049267904175552,3042374792655995433
