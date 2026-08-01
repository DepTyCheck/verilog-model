-- Seed: 12083926055304910821,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (wivi : in boolean_vector(4 downto 2); citeyhmo : buffer string(3 downto 2); qbxppgoef : in integer; xmbax : in std_logic);
end h;

architecture wajfz of h is
  
begin
  -- Single-driven assignments
  citeyhmo <= citeyhmo;
end wajfz;

library ieee;
use ieee.std_logic_1164.all;

entity xwitcwict is
  port (osfdgwoqro : out std_logic_vector(4 downto 2); wyslaqflie : in std_logic);
end xwitcwict;

library ieee;
use ieee.std_logic_1164.all;

architecture khffdazlnc of xwitcwict is
  signal bqrq : std_logic;
  signal qeb : string(3 downto 2);
  signal qxtbn : boolean_vector(4 downto 2);
  signal qu : std_logic;
  signal oi : string(3 downto 2);
  signal rurgbdwda : boolean_vector(4 downto 2);
  signal mkhwzpo : std_logic;
  signal w : string(3 downto 2);
  signal ske : integer;
  signal vpe : string(3 downto 2);
  signal txx : boolean_vector(4 downto 2);
begin
  uyw : entity work.h
    port map (wivi => txx, citeyhmo => vpe, qbxppgoef => ske, xmbax => wyslaqflie);
  qpuwc : entity work.h
    port map (wivi => txx, citeyhmo => w, qbxppgoef => ske, xmbax => mkhwzpo);
  krdp : entity work.h
    port map (wivi => rurgbdwda, citeyhmo => oi, qbxppgoef => ske, xmbax => qu);
  lcbmulnb : entity work.h
    port map (wivi => qxtbn, citeyhmo => qeb, qbxppgoef => ske, xmbax => bqrq);
  
  -- Single-driven assignments
  txx <= qxtbn;
  
  -- Multi-driven assignments
  osfdgwoqro <= ('1', 'U', '1');
  mkhwzpo <= 'Z';
  qu <= wyslaqflie;
  mkhwzpo <= wyslaqflie;
end khffdazlnc;



-- Seed after: 12237549808520321494,4292249356257567981
