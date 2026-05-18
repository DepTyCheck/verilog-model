-- Seed: 3486234888866264700,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity jcd is
  port (ukacwlqdh : out std_logic_vector(1 downto 2); pguncs : buffer integer);
end jcd;



architecture qaxa of jcd is
  
begin
  
end qaxa;

library ieee;
use ieee.std_logic_1164.all;

entity jwpswbbr is
  port (eyulqlbsu : buffer std_logic_vector(2 to 0); npff : in integer_vector(1 downto 2); vptmnft : out bit_vector(0 to 1));
end jwpswbbr;

library ieee;
use ieee.std_logic_1164.all;

architecture gizclggjb of jwpswbbr is
  signal jllqy : integer;
  signal ps : integer;
  signal ybeulrhfbt : std_logic_vector(1 downto 2);
  signal lhhjteiza : integer;
begin
  jxkt : entity work.jcd
    port map (ukacwlqdh => eyulqlbsu, pguncs => lhhjteiza);
  vfkpvwnbim : entity work.jcd
    port map (ukacwlqdh => ybeulrhfbt, pguncs => ps);
  be : entity work.jcd
    port map (ukacwlqdh => eyulqlbsu, pguncs => jllqy);
end gizclggjb;

library ieee;
use ieee.std_logic_1164.all;

entity cvzk is
  port (n : linkage integer; erfpafzgwi : in severity_level; obd : out std_logic);
end cvzk;

library ieee;
use ieee.std_logic_1164.all;

architecture zahipawyo of cvzk is
  signal zfqsqp : integer;
  signal xtcta : integer;
  signal xbdu : std_logic_vector(1 downto 2);
  signal sbp : bit_vector(0 to 1);
  signal qvfrl : integer_vector(1 downto 2);
  signal zmwp : bit_vector(0 to 1);
  signal w : integer_vector(1 downto 2);
  signal feneyldgb : std_logic_vector(1 downto 2);
begin
  psbbqmq : entity work.jwpswbbr
    port map (eyulqlbsu => feneyldgb, npff => w, vptmnft => zmwp);
  hftqvb : entity work.jwpswbbr
    port map (eyulqlbsu => feneyldgb, npff => qvfrl, vptmnft => sbp);
  rvxenana : entity work.jcd
    port map (ukacwlqdh => xbdu, pguncs => xtcta);
  yihx : entity work.jcd
    port map (ukacwlqdh => feneyldgb, pguncs => zfqsqp);
end zahipawyo;



-- Seed after: 164788546933243262,18238119570016518405
