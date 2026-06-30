-- Seed: 2609043931532072997,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity jfboltusz is
  port (nylhfk : linkage severity_level; xeyvl : inout real; hjowclo : out std_logic; pxertye : out std_logic_vector(2 downto 4));
end jfboltusz;

architecture apylql of jfboltusz is
  
begin
  -- Multi-driven assignments
  hjowclo <= '1';
  hjowclo <= 'H';
  pxertye <= (others => '0');
end apylql;

entity qcad is
  port (bhnqmlme : inout boolean);
end qcad;

library ieee;
use ieee.std_logic_1164.all;

architecture qbnmr of qcad is
  signal w : std_logic_vector(2 downto 4);
  signal p : std_logic;
  signal hybojlng : real;
  signal yaipzmjq : severity_level;
  signal rlrdhuhuz : real;
  signal kqfazr : severity_level;
  signal v : std_logic_vector(2 downto 4);
  signal xixxi : std_logic;
  signal tavpz : real;
  signal zglqbiwsj : severity_level;
begin
  sst : entity work.jfboltusz
    port map (nylhfk => zglqbiwsj, xeyvl => tavpz, hjowclo => xixxi, pxertye => v);
  fbgcfcnax : entity work.jfboltusz
    port map (nylhfk => kqfazr, xeyvl => rlrdhuhuz, hjowclo => xixxi, pxertye => v);
  dmahwvkhx : entity work.jfboltusz
    port map (nylhfk => yaipzmjq, xeyvl => hybojlng, hjowclo => p, pxertye => w);
  
  -- Multi-driven assignments
  xixxi <= 'X';
  xixxi <= 'W';
  xixxi <= 'L';
end qbnmr;



-- Seed after: 3028323138098195088,14629254427735353553
