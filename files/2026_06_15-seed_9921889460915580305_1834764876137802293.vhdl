-- Seed: 9921889460915580305,1834764876137802293

entity tpru is
  port (pytm : buffer integer);
end tpru;

architecture sn of tpru is
  
begin
  -- Single-driven assignments
  pytm <= 16#A#;
end sn;

library ieee;
use ieee.std_logic_1164.all;

entity xggg is
  port (ef : buffer std_logic);
end xggg;

architecture cuxmkqhe of xggg is
  signal fbeybljr : integer;
  signal l : integer;
  signal w : integer;
  signal faqjkqwvi : integer;
begin
  uvauubha : entity work.tpru
    port map (pytm => faqjkqwvi);
  dqpvz : entity work.tpru
    port map (pytm => w);
  lnw : entity work.tpru
    port map (pytm => l);
  ofcrzzfm : entity work.tpru
    port map (pytm => fbeybljr);
  
  -- Multi-driven assignments
  ef <= '0';
  ef <= 'U';
  ef <= 'Z';
end cuxmkqhe;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (nze : linkage std_logic; cyhu : linkage std_logic_vector(2 to 2); ngfm : buffer std_logic);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture jbff of r is
  signal ojm : integer;
  signal oisfo : std_logic;
  signal dybfl : integer;
  signal nethuj : integer;
begin
  xjm : entity work.tpru
    port map (pytm => nethuj);
  ij : entity work.tpru
    port map (pytm => dybfl);
  lk : entity work.xggg
    port map (ef => oisfo);
  quyrfv : entity work.tpru
    port map (pytm => ojm);
  
  -- Multi-driven assignments
  oisfo <= 'U';
  ngfm <= 'U';
end jbff;

entity p is
  port (bcitbigmcn : inout real);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture xess of p is
  signal rxrznmx : std_logic_vector(2 to 2);
  signal gmbtdrynl : std_logic;
  signal pkleg : integer;
begin
  isylfxa : entity work.tpru
    port map (pytm => pkleg);
  ix : entity work.r
    port map (nze => gmbtdrynl, cyhu => rxrznmx, ngfm => gmbtdrynl);
  
  -- Single-driven assignments
  bcitbigmcn <= 2_1.22221;
  
  -- Multi-driven assignments
  gmbtdrynl <= '1';
end xess;



-- Seed after: 14690482314848578572,1834764876137802293
