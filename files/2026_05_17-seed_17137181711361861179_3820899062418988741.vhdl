-- Seed: 17137181711361861179,3820899062418988741

library ieee;
use ieee.std_logic_1164.all;

entity qec is
  port (yqhirjz : in integer; krnba : linkage std_logic);
end qec;



architecture kj of qec is
  
begin
  
end kj;



entity tpf is
  port (zwsqcmebel : inout time; anmyf : buffer time; p : inout real; viuvqqwh : linkage real);
end tpf;



architecture xnyd of tpf is
  
begin
  
end xnyd;

library ieee;
use ieee.std_logic_1164.all;

entity kphkfjbhrj is
  port (qdyr : buffer time; hvjwql : out std_logic; kt : in integer);
end kphkfjbhrj;

library ieee;
use ieee.std_logic_1164.all;

architecture oyzkqirzq of kphkfjbhrj is
  signal jegrrfqzfe : std_logic;
  signal ghoelong : integer;
  signal bixo : time;
  signal jywhkdumo : real;
  signal yvv : real;
  signal twxfevqbgr : time;
  signal poncb : time;
begin
  o : entity work.tpf
    port map (zwsqcmebel => poncb, anmyf => twxfevqbgr, p => yvv, viuvqqwh => jywhkdumo);
  hadk : entity work.tpf
    port map (zwsqcmebel => bixo, anmyf => qdyr, p => jywhkdumo, viuvqqwh => yvv);
  lylyunvyjw : entity work.qec
    port map (yqhirjz => ghoelong, krnba => jegrrfqzfe);
  ilitdkw : entity work.qec
    port map (yqhirjz => kt, krnba => hvjwql);
end oyzkqirzq;

library ieee;
use ieee.std_logic_1164.all;

entity lb is
  port (ykuwxvaufe : linkage std_logic; tevzxuia : in time; hngsj : linkage character; qgmyd : in boolean);
end lb;

library ieee;
use ieee.std_logic_1164.all;

architecture rq of lb is
  signal xhrlya : std_logic;
  signal domnttbb : std_logic;
  signal shnsjiwyq : time;
  signal zvxtr : std_logic;
  signal mfjodsvxmi : integer;
  signal eqi : std_logic;
  signal rbufppze : time;
begin
  czcsgw : entity work.kphkfjbhrj
    port map (qdyr => rbufppze, hvjwql => eqi, kt => mfjodsvxmi);
  efaey : entity work.qec
    port map (yqhirjz => mfjodsvxmi, krnba => zvxtr);
  fplpqw : entity work.kphkfjbhrj
    port map (qdyr => shnsjiwyq, hvjwql => domnttbb, kt => mfjodsvxmi);
  bn : entity work.qec
    port map (yqhirjz => mfjodsvxmi, krnba => xhrlya);
end rq;



-- Seed after: 761518468144474838,3820899062418988741
