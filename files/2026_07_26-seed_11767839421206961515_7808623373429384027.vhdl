-- Seed: 11767839421206961515,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity aiw is
  port (xzgvyi : linkage time; l : in std_logic_vector(0 downto 0));
end aiw;

architecture crakeoxcg of aiw is
  
begin
  
end crakeoxcg;

entity poftvkek is
  port (gz : buffer integer; oz : buffer integer);
end poftvkek;

library ieee;
use ieee.std_logic_1164.all;

architecture u of poftvkek is
  signal ophgxdqcj : time;
  signal rmgjbpmm : std_logic_vector(0 downto 0);
  signal onby : time;
  signal qxybuzapxu : std_logic_vector(0 downto 0);
  signal kqpjsdkj : time;
begin
  yydc : entity work.aiw
    port map (xzgvyi => kqpjsdkj, l => qxybuzapxu);
  losp : entity work.aiw
    port map (xzgvyi => onby, l => rmgjbpmm);
  ghtrtygk : entity work.aiw
    port map (xzgvyi => ophgxdqcj, l => qxybuzapxu);
  
  -- Single-driven assignments
  oz <= gz;
  gz <= oz;
  
  -- Multi-driven assignments
  rmgjbpmm <= "H";
end u;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (r : in integer_vector(2 to 3); zdfs : in std_logic; eifwsr : in integer; rcpsdqh : linkage std_logic_vector(4 downto 0));
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture ypww of a is
  signal zxarwd : time;
  signal letn : time;
  signal nyla : std_logic_vector(0 downto 0);
  signal auy : time;
  signal p : integer;
  signal spelyp : integer;
begin
  xukcdwczm : entity work.poftvkek
    port map (gz => spelyp, oz => p);
  ihkqgjg : entity work.aiw
    port map (xzgvyi => auy, l => nyla);
  vf : entity work.aiw
    port map (xzgvyi => letn, l => nyla);
  shfdrenx : entity work.aiw
    port map (xzgvyi => zxarwd, l => nyla);
  
  -- Multi-driven assignments
  nyla <= nyla;
  nyla <= "H";
end ypww;

entity kxstqhny is
  port (x : out real; ufuxv : out time; kgrmnok : inout character; knriqb : in time);
end kxstqhny;

library ieee;
use ieee.std_logic_1164.all;

architecture vbc of kxstqhny is
  signal stgmpm : integer;
  signal jek : std_logic_vector(0 downto 0);
  signal weph : time;
  signal ciumglbm : std_logic_vector(4 downto 0);
  signal wlfu : integer;
  signal dnmzdt : std_logic;
  signal uun : integer_vector(2 to 3);
begin
  b : entity work.a
    port map (r => uun, zdfs => dnmzdt, eifwsr => wlfu, rcpsdqh => ciumglbm);
  ugahshto : entity work.aiw
    port map (xzgvyi => weph, l => jek);
  isxokuge : entity work.poftvkek
    port map (gz => stgmpm, oz => wlfu);
  gxm : entity work.aiw
    port map (xzgvyi => ufuxv, l => jek);
end vbc;



-- Seed after: 7500965665638943153,7808623373429384027
