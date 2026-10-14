-- Seed: 14256114497296498103,2287986438313874379

entity mlhl is
  port (pcspkscnxn : linkage time; xepn : in string(2 downto 4));
end mlhl;

architecture fepyd of mlhl is
  
begin
  
end fepyd;

library ieee;
use ieee.std_logic_1164.all;

entity uifo is
  port (usexcpwf : buffer integer; pqkerue : linkage std_logic_vector(4 to 4); voymbni : in integer);
end uifo;

architecture rbtfp of uifo is
  signal c : string(2 downto 4);
  signal zdld : time;
begin
  xmfor : entity work.mlhl
    port map (pcspkscnxn => zdld, xepn => c);
end rbtfp;

library ieee;
use ieee.std_logic_1164.all;

entity yuaypg is
  port (oiwwa : in std_logic_vector(2 to 2); borlgatiep : linkage integer; yoipv : buffer character);
end yuaypg;

architecture ewdxhwe of yuaypg is
  signal iy : string(2 downto 4);
  signal n : time;
begin
  oxfdsnyy : entity work.mlhl
    port map (pcspkscnxn => n, xepn => iy);
  
  -- Single-driven assignments
  iy <= "";
  yoipv <= yoipv;
end ewdxhwe;

library ieee;
use ieee.std_logic_1164.all;

entity qhv is
  port (jqzhlmfnq : inout std_logic; fsfhhttlbv : inout time; gip : in integer);
end qhv;

library ieee;
use ieee.std_logic_1164.all;

architecture erjlqe of qhv is
  signal zrbnybc : character;
  signal zfcw : std_logic_vector(2 to 2);
  signal zlttnhikq : integer;
  signal abxphqcqx : std_logic_vector(4 to 4);
  signal zg : integer;
begin
  tjrsp : entity work.uifo
    port map (usexcpwf => zg, pqkerue => abxphqcqx, voymbni => zlttnhikq);
  wixvxi : entity work.yuaypg
    port map (oiwwa => zfcw, borlgatiep => zlttnhikq, yoipv => zrbnybc);
  
  -- Single-driven assignments
  fsfhhttlbv <= 1304 ns;
  
  -- Multi-driven assignments
  jqzhlmfnq <= 'Z';
  jqzhlmfnq <= 'Z';
  jqzhlmfnq <= 'W';
  jqzhlmfnq <= jqzhlmfnq;
end erjlqe;



-- Seed after: 5371325557498916026,2287986438313874379
