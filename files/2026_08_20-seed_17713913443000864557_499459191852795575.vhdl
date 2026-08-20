-- Seed: 17713913443000864557,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity bgtx is
  port (t : in integer; qkzjjt : inout std_logic_vector(3 downto 0));
end bgtx;

architecture bmb of bgtx is
  
begin
  
end bmb;

entity sbgif is
  port (jfuvvhj : in severity_level; xvvoopozoa : linkage time; mpviz : inout time_vector(0 downto 4));
end sbgif;

library ieee;
use ieee.std_logic_1164.all;

architecture zl of sbgif is
  signal dek : std_logic_vector(3 downto 0);
  signal yvzjdde : integer;
begin
  qcvouhx : entity work.bgtx
    port map (t => yvzjdde, qkzjjt => dek);
  
  -- Multi-driven assignments
  dek <= "LXLX";
  dek <= "WXHZ";
end zl;

entity vbf is
  port (jk : out integer);
end vbf;

library ieee;
use ieee.std_logic_1164.all;

architecture kqq of vbf is
  signal yb : std_logic_vector(3 downto 0);
  signal hphaj : integer;
  signal gs : std_logic_vector(3 downto 0);
  signal ymw : integer;
begin
  rfrxdyyd : entity work.bgtx
    port map (t => ymw, qkzjjt => gs);
  gpags : entity work.bgtx
    port map (t => hphaj, qkzjjt => yb);
  
  -- Single-driven assignments
  hphaj <= ymw;
  ymw <= jk;
  jk <= jk;
  
  -- Multi-driven assignments
  gs <= ('X', 'X', '0', 'H');
  gs <= "XWLW";
  gs <= gs;
  gs <= ('W', 'X', '-', 'U');
end kqq;



-- Seed after: 593166267074070851,499459191852795575
