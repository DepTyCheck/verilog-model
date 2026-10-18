-- Seed: 8898630704064798527,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity bwtdmqf is
  port (afm : out time_vector(3 downto 0); qa : in std_logic_vector(4 downto 1); njfnzf : in std_logic; gwixwr : out bit);
end bwtdmqf;

architecture smhooj of bwtdmqf is
  
begin
  -- Single-driven assignments
  gwixwr <= '0';
end smhooj;

library ieee;
use ieee.std_logic_1164.all;

entity gyjnquf is
  port (afdrqm : in integer; v : inout std_logic; zhvltpc : inout std_logic; vtosygi : linkage bit);
end gyjnquf;

library ieee;
use ieee.std_logic_1164.all;

architecture vhogmixgm of gyjnquf is
  signal mluu : bit;
  signal kjz : std_logic;
  signal irvhys : std_logic_vector(4 downto 1);
  signal elq : time_vector(3 downto 0);
  signal rdbaxvufvf : bit;
  signal svwph : std_logic_vector(4 downto 1);
  signal mn : time_vector(3 downto 0);
  signal jslztjb : bit;
  signal pi : time_vector(3 downto 0);
  signal sv : bit;
  signal fvjwe : std_logic_vector(4 downto 1);
  signal cvyiiaen : time_vector(3 downto 0);
begin
  t : entity work.bwtdmqf
    port map (afm => cvyiiaen, qa => fvjwe, njfnzf => v, gwixwr => sv);
  fhpd : entity work.bwtdmqf
    port map (afm => pi, qa => fvjwe, njfnzf => zhvltpc, gwixwr => jslztjb);
  bvsulnviip : entity work.bwtdmqf
    port map (afm => mn, qa => svwph, njfnzf => zhvltpc, gwixwr => rdbaxvufvf);
  rze : entity work.bwtdmqf
    port map (afm => elq, qa => irvhys, njfnzf => kjz, gwixwr => mluu);
  
  -- Multi-driven assignments
  svwph <= fvjwe;
end vhogmixgm;

library ieee;
use ieee.std_logic_1164.all;

entity gjoczwirdl is
  port (b : linkage real; g : out std_logic_vector(4 downto 2); euhqrfjxz : buffer real);
end gjoczwirdl;

library ieee;
use ieee.std_logic_1164.all;

architecture ofdpfdel of gjoczwirdl is
  signal vfhqfghvbd : bit;
  signal ukvpxcnvft : std_logic;
  signal ck : integer;
begin
  zshdyctur : entity work.gyjnquf
    port map (afdrqm => ck, v => ukvpxcnvft, zhvltpc => ukvpxcnvft, vtosygi => vfhqfghvbd);
  
  -- Single-driven assignments
  euhqrfjxz <= 2_2_1_3.0_0_0_2_4;
  ck <= ck;
end ofdpfdel;



-- Seed after: 340960613440639834,14108379317686958507
