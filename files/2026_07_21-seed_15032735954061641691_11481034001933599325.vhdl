-- Seed: 15032735954061641691,11481034001933599325

entity pipwp is
  port (vzusfrn : in real);
end pipwp;

architecture zt of pipwp is
  
begin
  
end zt;

library ieee;
use ieee.std_logic_1164.all;

entity iktvoicutp is
  port (bupgawelt : in integer_vector(4 downto 3); uvuyfwyra : linkage integer; elss : in std_logic; brlrfefhll : linkage integer);
end iktvoicutp;

architecture yhpurrns of iktvoicutp is
  signal adxnzxfa : real;
  signal tzkkxkge : real;
begin
  lkkwps : entity work.pipwp
    port map (vzusfrn => tzkkxkge);
  us : entity work.pipwp
    port map (vzusfrn => adxnzxfa);
  vxcennosb : entity work.pipwp
    port map (vzusfrn => tzkkxkge);
  jxbjrxo : entity work.pipwp
    port map (vzusfrn => tzkkxkge);
  
  -- Single-driven assignments
  tzkkxkge <= 2#00011.0_1#;
  adxnzxfa <= adxnzxfa;
end yhpurrns;

entity lly is
  port (ew : buffer integer);
end lly;

library ieee;
use ieee.std_logic_1164.all;

architecture ncgpv of lly is
  signal gukwna : real;
  signal qdwdeut : std_logic;
  signal iunxnbk : integer;
  signal khs : integer_vector(4 downto 3);
begin
  hvxjwfzz : entity work.iktvoicutp
    port map (bupgawelt => khs, uvuyfwyra => iunxnbk, elss => qdwdeut, brlrfefhll => ew);
  uxpafm : entity work.pipwp
    port map (vzusfrn => gukwna);
  
  -- Single-driven assignments
  gukwna <= gukwna;
  
  -- Multi-driven assignments
  qdwdeut <= '1';
  qdwdeut <= qdwdeut;
end ncgpv;



-- Seed after: 11736072336649438691,11481034001933599325
