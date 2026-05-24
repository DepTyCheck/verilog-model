-- Seed: 1866584698031056762,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity myk is
  port (cfrjymrvf : out std_logic_vector(0 to 2); jvlwyq : in std_logic);
end myk;



architecture aiupjtzcns of myk is
  
begin
  
end aiupjtzcns;



entity yqe is
  port (gf : buffer real_vector(3 downto 3); uxrzncoqpq : inout real; q : out real);
end yqe;

library ieee;
use ieee.std_logic_1164.all;

architecture ikskvnsgxj of yqe is
  signal nnq : std_logic;
  signal jiohxmgmwe : std_logic_vector(0 to 2);
  signal hbijnsstu : std_logic;
  signal gnjpnzpqkp : std_logic_vector(0 to 2);
  signal baysklw : std_logic;
  signal imyy : std_logic_vector(0 to 2);
begin
  a : entity work.myk
    port map (cfrjymrvf => imyy, jvlwyq => baysklw);
  mhdwxnx : entity work.myk
    port map (cfrjymrvf => gnjpnzpqkp, jvlwyq => hbijnsstu);
  mag : entity work.myk
    port map (cfrjymrvf => imyy, jvlwyq => hbijnsstu);
  dckaal : entity work.myk
    port map (cfrjymrvf => jiohxmgmwe, jvlwyq => nnq);
end ikskvnsgxj;



entity ywrby is
  port (avcphzsfg : inout integer; uuo : in bit_vector(1 downto 3));
end ywrby;

library ieee;
use ieee.std_logic_1164.all;

architecture ajigqsxqm of ywrby is
  signal ijlmny : std_logic;
  signal nmybesve : std_logic;
  signal ihtqxprhjy : std_logic_vector(0 to 2);
  signal tetoyzdmu : real;
  signal c : real;
  signal emhaq : real_vector(3 downto 3);
begin
  knh : entity work.yqe
    port map (gf => emhaq, uxrzncoqpq => c, q => tetoyzdmu);
  rmbkkwf : entity work.myk
    port map (cfrjymrvf => ihtqxprhjy, jvlwyq => nmybesve);
  yadiohe : entity work.myk
    port map (cfrjymrvf => ihtqxprhjy, jvlwyq => ijlmny);
end ajigqsxqm;



-- Seed after: 6006628741365632117,11387579217500963635
