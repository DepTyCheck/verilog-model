-- Seed: 3397130603239545207,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity ufdb is
  port (cgddsmgb : in std_logic; arl : linkage integer);
end ufdb;

architecture jlwir of ufdb is
  
begin
  
end jlwir;

library ieee;
use ieee.std_logic_1164.all;

entity tgjzdlnb is
  port (smj : out bit; dnjbx : linkage integer; zvui : buffer real; hocucwkna : buffer std_logic_vector(2 downto 2));
end tgjzdlnb;

library ieee;
use ieee.std_logic_1164.all;

architecture uc of tgjzdlnb is
  signal jzid : integer;
  signal txyjmmhew : integer;
  signal ougkfshfon : std_logic;
begin
  phovwwaano : entity work.ufdb
    port map (cgddsmgb => ougkfshfon, arl => txyjmmhew);
  wvd : entity work.ufdb
    port map (cgddsmgb => ougkfshfon, arl => jzid);
  
  -- Single-driven assignments
  zvui <= zvui;
  smj <= smj;
end uc;

library ieee;
use ieee.std_logic_1164.all;

entity svf is
  port (cvhugt : in std_logic);
end svf;

library ieee;
use ieee.std_logic_1164.all;

architecture thybd of svf is
  signal kks : integer;
  signal ndubonsxa : real;
  signal gncczll : integer;
  signal ue : bit;
  signal xegc : integer;
  signal rdlqtqctot : std_logic_vector(2 downto 2);
  signal bcyxjibfin : real;
  signal furqvelkv : integer;
  signal m : bit;
begin
  yutva : entity work.tgjzdlnb
    port map (smj => m, dnjbx => furqvelkv, zvui => bcyxjibfin, hocucwkna => rdlqtqctot);
  kbjzwbn : entity work.ufdb
    port map (cgddsmgb => cvhugt, arl => xegc);
  frzx : entity work.tgjzdlnb
    port map (smj => ue, dnjbx => gncczll, zvui => ndubonsxa, hocucwkna => rdlqtqctot);
  krldbddy : entity work.ufdb
    port map (cgddsmgb => cvhugt, arl => kks);
  
  -- Multi-driven assignments
  rdlqtqctot <= (others => 'H');
  rdlqtqctot <= (others => 'L');
end thybd;



-- Seed after: 8634571822224379439,4177195558088809003
