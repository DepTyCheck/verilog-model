-- Seed: 17863721042074974712,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity roihurp is
  port (ocq : linkage std_logic_vector(2 downto 0); vjdng : in std_logic_vector(0 to 3); vkkd : out std_logic);
end roihurp;

architecture podq of roihurp is
  
begin
  -- Multi-driven assignments
  vkkd <= 'L';
  vkkd <= 'X';
  vkkd <= 'L';
  vkkd <= 'U';
end podq;

entity jdlzmstj is
  port (xyvm : in real_vector(2 downto 3); yelucyruie : linkage real);
end jdlzmstj;

library ieee;
use ieee.std_logic_1164.all;

architecture wzf of jdlzmstj is
  signal jdmakmwxv : std_logic;
  signal qquznftz : std_logic_vector(0 to 3);
  signal rlcap : std_logic;
  signal btbjhkpcey : std_logic_vector(0 to 3);
  signal dql : std_logic_vector(2 downto 0);
begin
  xcizzqr : entity work.roihurp
    port map (ocq => dql, vjdng => btbjhkpcey, vkkd => rlcap);
  hcyjynl : entity work.roihurp
    port map (ocq => dql, vjdng => qquznftz, vkkd => jdmakmwxv);
  
  -- Multi-driven assignments
  qquznftz <= btbjhkpcey;
  rlcap <= '1';
end wzf;

library ieee;
use ieee.std_logic_1164.all;

entity dbtuqulbl is
  port (gnkgtq : out bit; upzpfvo : linkage std_logic_vector(3 downto 3));
end dbtuqulbl;

library ieee;
use ieee.std_logic_1164.all;

architecture pyqagjmi of dbtuqulbl is
  signal pt : std_logic;
  signal rjcjcoxe : std_logic_vector(0 to 3);
  signal e : std_logic;
  signal c : std_logic_vector(0 to 3);
  signal xaaivzaq : std_logic_vector(2 downto 0);
begin
  wcxkxov : entity work.roihurp
    port map (ocq => xaaivzaq, vjdng => c, vkkd => e);
  w : entity work.roihurp
    port map (ocq => xaaivzaq, vjdng => rjcjcoxe, vkkd => pt);
  
  -- Single-driven assignments
  gnkgtq <= '0';
  
  -- Multi-driven assignments
  xaaivzaq <= xaaivzaq;
end pyqagjmi;



-- Seed after: 11457845608235050768,14108379317686958507
