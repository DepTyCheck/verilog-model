-- Seed: 11779155756649550794,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity rze is
  port (s : inout std_logic_vector(4 downto 3); hkcf : buffer integer_vector(2 downto 3));
end rze;

architecture bom of rze is
  
begin
  -- Single-driven assignments
  hkcf <= hkcf;
  
  -- Multi-driven assignments
  s <= "UX";
  s <= ('-', 'Z');
  s <= "1W";
  s <= s;
end bom;

library ieee;
use ieee.std_logic_1164.all;

entity topqya is
  port (vtu : buffer std_logic; cbkpywz : in real; ytbvay : in integer);
end topqya;

library ieee;
use ieee.std_logic_1164.all;

architecture guoeqsstu of topqya is
  signal gfcgkkrnk : integer_vector(2 downto 3);
  signal ehh : std_logic_vector(4 downto 3);
begin
  mb : entity work.rze
    port map (s => ehh, hkcf => gfcgkkrnk);
end guoeqsstu;

library ieee;
use ieee.std_logic_1164.all;

entity cwx is
  port (ydyjalno : linkage std_logic);
end cwx;

library ieee;
use ieee.std_logic_1164.all;

architecture ofmhsp of cwx is
  signal jpjifil : real;
  signal grw : std_logic;
  signal aalvzc : integer_vector(2 downto 3);
  signal xdfc : std_logic_vector(4 downto 3);
  signal snmltnycq : integer_vector(2 downto 3);
  signal ee : std_logic_vector(4 downto 3);
  signal kgzmon : integer;
  signal jrnxbrbn : real;
  signal rhupmnrghg : std_logic;
begin
  uufhmu : entity work.topqya
    port map (vtu => rhupmnrghg, cbkpywz => jrnxbrbn, ytbvay => kgzmon);
  oxwbyrg : entity work.rze
    port map (s => ee, hkcf => snmltnycq);
  jmqwoy : entity work.rze
    port map (s => xdfc, hkcf => aalvzc);
  rffhtnrksx : entity work.topqya
    port map (vtu => grw, cbkpywz => jpjifil, ytbvay => kgzmon);
  
  -- Single-driven assignments
  jrnxbrbn <= 2_1_4_3_2.0;
  jpjifil <= jrnxbrbn;
  kgzmon <= kgzmon;
  
  -- Multi-driven assignments
  rhupmnrghg <= rhupmnrghg;
  rhupmnrghg <= 'U';
end ofmhsp;

library ieee;
use ieee.std_logic_1164.all;

entity tidowyc is
  port (zeqmuqj : out std_logic_vector(3 downto 2));
end tidowyc;

library ieee;
use ieee.std_logic_1164.all;

architecture rgolagfbtd of tidowyc is
  signal jfcnym : std_logic;
  signal ftkbhtkl : integer;
  signal xxrperhryl : real;
  signal bmsaa : std_logic;
  signal mzrabl : std_logic;
  signal bkbmjcubq : integer;
  signal arvho : real;
  signal nx : std_logic;
begin
  vst : entity work.topqya
    port map (vtu => nx, cbkpywz => arvho, ytbvay => bkbmjcubq);
  wbi : entity work.cwx
    port map (ydyjalno => mzrabl);
  zhwfkype : entity work.topqya
    port map (vtu => bmsaa, cbkpywz => xxrperhryl, ytbvay => ftkbhtkl);
  aqmxwsmseo : entity work.topqya
    port map (vtu => jfcnym, cbkpywz => arvho, ytbvay => ftkbhtkl);
end rgolagfbtd;



-- Seed after: 7021741701460108619,4245627776430562977
