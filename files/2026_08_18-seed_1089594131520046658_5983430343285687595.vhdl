-- Seed: 1089594131520046658,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity cdlh is
  port ( ffjwifio : buffer std_logic_vector(2 downto 1)
  ; a : buffer integer
  ; ggurbqjqcv : out std_logic_vector(0 to 2)
  ; s : linkage time_vector(4 downto 0)
  );
end cdlh;

architecture zxj of cdlh is
  
begin
  -- Multi-driven assignments
  ggurbqjqcv <= ggurbqjqcv;
end zxj;

library ieee;
use ieee.std_logic_1164.all;

entity mawjg is
  port (hkocawowfh : inout std_logic; xwofraiwmj : out std_logic_vector(4 downto 4); ewirydlu : linkage real);
end mawjg;

architecture puxmh of mawjg is
  
begin
  -- Multi-driven assignments
  xwofraiwmj <= xwofraiwmj;
  hkocawowfh <= 'X';
  xwofraiwmj <= xwofraiwmj;
end puxmh;

entity jeot is
  port (iamzegucfc : buffer boolean_vector(2 downto 0); huvpwhdrw : in time; hocrxs : out boolean);
end jeot;

library ieee;
use ieee.std_logic_1164.all;

architecture h of jeot is
  signal itbrvbqks : time_vector(4 downto 0);
  signal s : integer;
  signal kg : std_logic_vector(2 downto 1);
  signal vippwbdfmq : time_vector(4 downto 0);
  signal jdyg : std_logic_vector(0 to 2);
  signal c : integer;
  signal ltnwuaeqm : std_logic_vector(2 downto 1);
begin
  stdcszglgo : entity work.cdlh
    port map (ffjwifio => ltnwuaeqm, a => c, ggurbqjqcv => jdyg, s => vippwbdfmq);
  dkvx : entity work.cdlh
    port map (ffjwifio => kg, a => s, ggurbqjqcv => jdyg, s => itbrvbqks);
  
  -- Single-driven assignments
  hocrxs <= hocrxs;
  iamzegucfc <= (FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  ltnwuaeqm <= ltnwuaeqm;
  kg <= ('Z', 'L');
  jdyg <= jdyg;
end h;

library ieee;
use ieee.std_logic_1164.all;

entity hiujk is
  port (vlvtqkrdeq : buffer real; djgmbpwt : inout bit_vector(1 to 0); f : inout std_logic_vector(4 downto 4));
end hiujk;

library ieee;
use ieee.std_logic_1164.all;

architecture pymvai of hiujk is
  signal pcvuuxb : time_vector(4 downto 0);
  signal dwkqakfpy : integer;
  signal nebafeacp : std_logic_vector(2 downto 1);
  signal y : time_vector(4 downto 0);
  signal vljfcirs : integer;
  signal ccfqoq : std_logic_vector(2 downto 1);
  signal jrirmlcvf : time_vector(4 downto 0);
  signal qt : std_logic_vector(0 to 2);
  signal jemo : integer;
  signal lihrhjwa : std_logic_vector(2 downto 1);
begin
  tfiluyo : entity work.cdlh
    port map (ffjwifio => lihrhjwa, a => jemo, ggurbqjqcv => qt, s => jrirmlcvf);
  q : entity work.cdlh
    port map (ffjwifio => ccfqoq, a => vljfcirs, ggurbqjqcv => qt, s => y);
  ctkfuv : entity work.cdlh
    port map (ffjwifio => nebafeacp, a => dwkqakfpy, ggurbqjqcv => qt, s => pcvuuxb);
  
  -- Single-driven assignments
  djgmbpwt <= (others => '0');
  vlvtqkrdeq <= vlvtqkrdeq;
  
  -- Multi-driven assignments
  f <= (others => 'Z');
end pymvai;



-- Seed after: 7811710669810335039,5983430343285687595
