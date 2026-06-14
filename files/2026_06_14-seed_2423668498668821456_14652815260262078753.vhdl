-- Seed: 2423668498668821456,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity sl is
  port (ptarfllr : inout std_logic_vector(0 downto 0); ip : buffer time);
end sl;

architecture i of sl is
  
begin
  -- Multi-driven assignments
  ptarfllr <= (others => 'H');
  ptarfllr <= "U";
  ptarfllr <= (others => 'W');
  ptarfllr <= (others => '-');
end i;

library ieee;
use ieee.std_logic_1164.all;

entity iagk is
  port (m : in std_logic);
end iagk;

library ieee;
use ieee.std_logic_1164.all;

architecture t of iagk is
  signal vollanf : time;
  signal nsb : std_logic_vector(0 downto 0);
begin
  emikdnm : entity work.sl
    port map (ptarfllr => nsb, ip => vollanf);
  
  -- Multi-driven assignments
  nsb <= (others => 'Z');
  nsb <= (others => 'L');
  nsb <= (others => 'X');
end t;

library ieee;
use ieee.std_logic_1164.all;

entity nkexeg is
  port (ahpjbmfpk : in std_logic_vector(4 to 4); kuroosehgg : in std_logic; iihzibw : buffer bit; jkozeeng : buffer std_logic);
end nkexeg;

library ieee;
use ieee.std_logic_1164.all;

architecture mhdxq of nkexeg is
  signal xri : time;
  signal qzcb : time;
  signal imy : time;
  signal n : std_logic_vector(0 downto 0);
begin
  qhuru : entity work.sl
    port map (ptarfllr => n, ip => imy);
  oxogkglz : entity work.sl
    port map (ptarfllr => n, ip => qzcb);
  t : entity work.iagk
    port map (m => jkozeeng);
  eeljxygr : entity work.sl
    port map (ptarfllr => n, ip => xri);
  
  -- Single-driven assignments
  iihzibw <= '1';
end mhdxq;

entity nyasfat is
  port (j : in time; cddfgtw : buffer integer; gvmfdv : linkage integer_vector(3 to 2));
end nyasfat;

library ieee;
use ieee.std_logic_1164.all;

architecture qnsvqpehxi of nyasfat is
  signal tgtse : std_logic;
  signal fvblke : std_logic;
  signal uzfcda : bit;
  signal evxncxpeh : std_logic;
  signal je : std_logic_vector(4 to 4);
begin
  autycno : entity work.nkexeg
    port map (ahpjbmfpk => je, kuroosehgg => evxncxpeh, iihzibw => uzfcda, jkozeeng => fvblke);
  ivlpcrnhgs : entity work.iagk
    port map (m => tgtse);
  aylzwfmmq : entity work.iagk
    port map (m => evxncxpeh);
  
  -- Single-driven assignments
  cddfgtw <= 2_0;
  
  -- Multi-driven assignments
  je <= "-";
end qnsvqpehxi;



-- Seed after: 3870047371815394433,14652815260262078753
